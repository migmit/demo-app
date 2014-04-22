package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.concurrent.Execution.Implicits._

import views._
import models._
import utils._

object SignUp extends Controller {
  
  /**
   * Sign Up Form definition.
   *
   * Once defined it handle automatically, ,
   * validation, submission, errors, redisplaying, ...
   */
  val signupForm: Form[User] = Form(
    
    // Define a mapping that will handle User values
    mapping(
      "username" -> text(minLength = 4).verifying(
          "Username should start from uppercase letter",
          _.head.isUpper
      ),
      "email" -> email,
      
      // Create a tuple mapping for the password/confirm
      "password" -> tuple(
        "main" -> text(minLength = 6),
        "confirm" -> text
      ).verifying(
        // Add an additional constraint: both passwords must match
        "Passwords don't match", passwords => passwords._1 == passwords._2
      ),
      
      // Create a mapping that will handle UserProfile values
      "profile" -> mapping(
        "country" -> nonEmptyText,
        "address" -> optional(text),
        "age" -> optional(number(min = 18, max = 100))
      )
      // The mapping signature matches the UserProfile case class signature,
      // so we can use default apply/unapply functions here
      (UserProfile.apply)(UserProfile.unapply),
      
      "accept" -> checked("You must accept the conditions")
      
    )
    // The mapping signature doesn't match the User case class signature,
    // so we have to define custom binding/unbinding functions
    {
      // Binding: Create a User from the mapping result (ignore the second password and the accept field)
      (username, email, passwords, profile, _) => User(username, passwords._1, email, profile) 
    } 
    {
      // Unbinding: Create the mapping values from an existing User value
      user => Some(user.username, user.email, (user.password, ""), user.profile, false)
    }.verifying(
      // Add an additional constraint: The username must not be taken (you could do an SQL request here)
      "This username is not available",
      user => !Seq("admin", "guest").contains(user.username)
    )
  )

  case class Credentials(
    name : String,
    password : String
  ) {
    def check : Future[Boolean] = {
      (
        for {
          _ <- FutureOpt.assert(RedisUser.isAdminName(name))
          pass <- FutureOpt.lift(RedisUser.adminPassword(name))
        } yield pass == Some(password)
      ) getOrElse false
    }
  }

  object Credentials {
    def fromSession(implicit request : RequestHeader) : Option[Credentials] = {
      for {
        name <- session.get("adminName")
        pass <- session.get("adminPass")
      } yield Credentials(name, pass)
    }
    def check(implicit request : RequestHeader) : Future[Boolean] = {
      fromSession match {
        case None => Future(false)
        case Some(c) => c.check
      }
    }
  }
  
  /**
   * Display an empty form.
   */
  def form = Action.async {implicit request =>
    (
      for {_ <- FutureOpt.assert(Credentials.check)}
      yield Ok(html.signup.form(signupForm))
    ) getOrElse Redirect(routes.SignUp.login(None))
  }
  
  /**
   * Display a form pre-filled with an existing User.
   */
  def editForm(username : String) = Action.async {implicit request =>
    (
      for {
        _ <- FutureOpt.assert(Credentials.check)
        response <- FutureOpt.lift((
          for {user <- FutureOpt(RedisUser.readUser(username))}
          yield Ok(html.signup.form(signupForm.fill(user)))
        ) getOrElse BadRequest(html.users.error(username)))
      } yield response
    ) getOrElse Redirect(routes.SignUp.login(Some(username)))
  }
  
  /**
   * Handle form submission.
   */
  def submit = Action.async { implicit request =>
    signupForm.bindFromRequest.fold(
      // Form has errors, redisplay it
      errors => Future(BadRequest(html.signup.form(errors))),
      
      // We got a valid User value, display the summary
      user => for {_ <- RedisUser.writeUser(user)} yield Ok(html.signup.summary(user))
    )
  }

  val loginForm : Form[Credentials] = Form(
    mapping("name" -> text, "password" -> text)(Credentials.apply)(Credentials.unapply)
  )
  
  def login(username : Option[String]) = Action {
    Ok(html.signup.login(username, loginForm))
  }

  def loginUnsuccesful(implicit request : RequestHeader) = {
    Redirect(routes.Application.index).withSession(session - "adminName" - "adminPass")
  }

  def logout = Action {implicit request => loginUnsuccesful}

  def loginSubmit(username : Option[String]) = Action.async {implicit request =>
    val credentials = loginForm.bindFromRequest.get
    credentials.check.map(isAdmin =>
      if (isAdmin) {
        val response = username match {
          case None => Redirect(routes.SignUp.form)
          case Some(name) => Redirect(routes.SignUp.editForm(name))
        }
        response.withSession(
          session +
            ("adminName" -> credentials.name) +
            ("adminPass" -> credentials.password)
        )
      } else loginUnsuccesful
    )
  }
}
