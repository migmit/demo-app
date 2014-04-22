package utils

import scala.concurrent._
import play.api.libs.concurrent.Execution.Implicits._

object FutureOpt {
  def assert(flag : Future[Boolean]) : FutureOpt[Unit] =
    FutureOpt(flag.map(if (_) Some(Unit) else None))
  def lift[R](future : Future[R]) : FutureOpt[R] = FutureOpt(future.map(Some(_)))
}

case class FutureOpt[R](
  impl : Future[Option[R]]
) {
  def get = impl
  def optional = FutureOpt(impl.map(Some(_)))
  def map[S](f : R => S) = FutureOpt(impl.map(_.map(f)))
  def flatMap[S](f : R => FutureOpt[S]) =
    FutureOpt(impl.flatMap(
      _ match {
        case None => Future(None)
        case Some(r) => f(r).get
      }
    ))
  def getOrElse(other : Future[R]) : Future[R] =
    impl.flatMap {_ match {
      case None => other
      case Some(r) => Future(r)
    }}
}
