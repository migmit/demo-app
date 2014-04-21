package controllers

import scala.concurrent._

import akka.util.ByteString

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import views._
import models._
import utils.RedisUser

object AllUsers extends Controller {
  def list = Action.async {
    val allUsers = RedisUser.allUserNames
    allUsers.flatMap {
      userNames =>
      Future.sequence(for(userName <- userNames) yield RedisUser.readUser(userName))
    }.map {users => Ok(html.users.list(users.flatten))}
  }
  def lookup(email : String) = Action.async {
    val allUsers = RedisUser.allUserNames
    allUsers.flatMap {
      userNames =>
      Future.sequence(for(userName <- userNames) yield RedisUser.readUser(userName))
    }.map {
      users =>
      val goodUsers = users.flatten.filter(user => user.email == email)
      Ok(html.users.lookup(email, goodUsers))
    }
  }
  def details(name : String) = Action.async {
    for (user <- RedisUser.readUser(name))
    yield user match {
      case Some(u) => Ok(html.users.details(u))
      case None => BadRequest(html.users.error(name))
    }
  }
  def json(name : String) = Action.async {
    for (user <- RedisUser.readUser(name))
    yield user match {
      case Some(u) => Ok(Json.obj(
        "username" -> u.username,
        "email" -> u.email,
        "password" -> u.password,
        "country" -> u.profile.country,
        "address" -> u.profile.address,
        "age" -> u.profile.age
      ))
      case None => BadRequest(Json.obj("wrong_name" -> name))
    }
  }
  def xml(name : String) = Action.async {
    for (user <- RedisUser.readUser(name))
    yield user match {
      case Some(u) => Ok(views.xml.users.details(u))
      case None => BadRequest(views.xml.users.error(name))
    }
  }
}
