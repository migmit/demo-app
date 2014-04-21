package utils

import scala.concurrent._
import akka.util.ByteString

import redis._
import play.api.libs.concurrent.Execution.Implicits._

import models._

case class FutureOpt[R](
  impl : Future[Option[R]]
) {
  def get = impl
  def optional = FutureOpt(impl.map(opt => Some(opt)))
  def map[S](f : R => S) = FutureOpt(impl.map(_.map(f)))
  def flatMap[S](f : R => FutureOpt[S]) =
    FutureOpt(impl.flatMap(
      _ match {
        case None => Future(None)
        case Some(r) => f(r).get
      }
    ))
}

object RedisUser {
  implicit val akkaSystem = akka.actor.ActorSystem()
  val redis = RedisClient()
  implicit object BStoInt extends ByteStringDeserializer[Int] {
    def deserialize(bs : ByteString) : Int = bs.utf8String.toInt
  }
  def readUser(name : String) : Future[Option[User]] = {
    val redisName = "user." + name
    val fUser =
      for (
        password <- FutureOpt(redis.hget[String](redisName, "password"));
        email <- FutureOpt(redis.hget[String](redisName, "email"));
        country <- FutureOpt(redis.hget[String](redisName, "country"));
        address <- FutureOpt(redis.hget[String](redisName, "address")).optional;
        age <- FutureOpt(redis.hget[Int](redisName, "age")).optional
      ) yield User(name, password, email, UserProfile(country, address, age))
    fUser.impl    
  }
  def writeUser(user : User) : Future[Unit] = {
    val redisName = "user." + user.username
    for (
      _ <- redis.hset(redisName, "password", user.password);
      _ <- redis.hset(redisName, "email", user.email);
      _ <- redis.hset(redisName, "country", user.profile.country);
      _ <- user.profile.address match {
        case Some(addr) => redis.hset(redisName, "address", addr)
        case None => redis.hdel(redisName, "address")
      };
      _ <- user.profile.age match {
        case Some(ag) => redis.hset(redisName, "age", ag)
        case None => redis.hdel(redisName, "age")
      };
      _ <- redis.sadd("users", user.username)
    ) yield Unit
  }
  def allUserNames : Future[Seq[String]] = redis.smembers[String]("users")
  def isAdminName(name : String) : Future[Boolean] = redis.sismember("admins", name)
  def allAdminNames : Future[Seq[String]] = redis.smembers[String]("admins")
  def adminPassword(admin : String) : Future[Option[String]] =
    redis.get[String]("admin." + admin)
}
