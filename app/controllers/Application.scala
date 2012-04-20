package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.templates._
import play.api.cache.Cache
import play.api.Play.current
import models._
import views._
import scala.util.Random
import java.security.SecureRandom


object Application extends Controller with Auth {

  val loginForm = Form {
    mapping("email" -> email, "password" -> text)(User.authenticate)(_.map(u => (u.email, "")))
      .verifying("Invalid email or password", result => result.isDefined)
  }

  def login = Action { implicit request =>
    Ok(html.login(loginForm))
  }

  def logout = Action { implicit request =>
    for {
      sessionId <- request.session.get("sessionId")
      userId <- Cache.getAs[String](sessionId + ":sessionId")
    } {
      Cache.set(sessionId + ":sessionId", "", 1)
      Cache.set(userId + ":userId", "", 1)
    }
    Redirect(routes.Application.login).withNewSession.flashing(
      "success" -> "You've been logged out"
    )
  }

  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.login(formWithErrors)),
      user => {
        val userId = user.get.id
        Cache.getAs[String](userId + ":userId").foreach { old =>
          Cache.set(old + ":sessionId", "", 1)
        }
        val sessionId = generateId()
        Cache.set(sessionId + ":sessionId", userId, sessionTimeoutInSeconds)
        Cache.set(userId + ":userId", sessionId, sessionTimeoutInSeconds)
        Redirect(routes.Message.main).withSession("sessionId" -> sessionId)
      }
    )
  }

}
object Message extends Controller with Auth with Pjax {

  private def compositeAction(actionType: ActionType)(f: User => Template => Request[Any] => Result) =
    Action { implicit request =>
      (for {
        user     <- authorize(actionType).right
        template <- pjax(user).right
      } yield f(user)(template)(request)).merge
    }

  def main = compositeAction(Read) { user => implicit template => implicit request =>
    val title = "hoge"
    Ok(html.message.main(title))
  }

}

trait Pjax {
  self: Controller =>

  type Template = String => Html => Html
  def pjax(user: User)(implicit request: Request[Any]): Either[Result, Template] = Right {
    if (request.headers.keys("X-PJAX")) html.pjaxTemplate.apply
    else html.fullTemplate.apply(user)
  }

}

trait Auth {
  self: Controller =>

  val sessionTimeoutInSeconds: Int = 3600

  def authorize(actionType: ActionType)(implicit request: Request[Any]): Either[Result, User] = for {
    user <- authenticated(request).right
    _    <- authorized(user, actionType).right
  } yield user

  private def authenticated(request: Request[Any]): Either[Result, User] =
    restoreUser(request).toRight(Redirect(routes.Application.login))

  private def restoreUser(request: Request[Any]): Option[User] = for {
    sessionId <- request.session.get("sessionId")
    userId <- Cache.getAs[String](sessionId + ":sessionId")
    user <- User.findById(userId)
  } yield {
    Cache.set(sessionId + ":sessionId", userId, sessionTimeoutInSeconds)
    Cache.set(userId + ":userId", sessionId, sessionTimeoutInSeconds)
    user
  }

  private def authorized(user: User, actionType: ActionType): Either[Result, Unit] =
    Either.cond(user.permission.executable(actionType), (), Forbidden("no permission"))

  private val random = new Random(new SecureRandom())

  def generateId(): String = {
    def isAlphaNum(c: Char) = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
    Stream.continually(random.nextPrintableChar).filter(isAlphaNum).take(64).mkString
  }

}