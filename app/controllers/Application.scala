package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.templates._
import play.api.cache.Cache
import play.api.Play.current
import models._
import views._
import scala.util.Random
import java.security.SecureRandom
import play.api.mvc._
import play.api.mvc.Results._


object Application extends Controller with LoginLogout with AuthConfigImpl {

  val loginForm = Form {
    mapping("email" -> email, "password" -> text)(User.authenticate)(_.map(u => (u.email, "")))
      .verifying("Invalid email or password", result => result.isDefined)
  }

  def login = Action { implicit request =>
    Ok(html.login(loginForm))
  }

  def logout = Action { request =>
    gotoLogoutSucceeded(request).flashing(
      "success" -> "You've been logged out"
    )
  }

  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.login(formWithErrors)),
      user => gotoLoginSucceeded(user.get.id)
    )
  }

}
object Message extends Base {

  def main = compositeAction(Read) { user => implicit template => implicit request =>
    val title = "message main"
    Ok(html.message.main(title))
  }

  def list = compositeAction(Read) { user => implicit template => implicit request =>
    val title = "all messages"
    Ok(html.message.list(title))
  }

  def detail(id: Int) = compositeAction(Read) { user => implicit template => implicit request =>
    val title = "messages detail "
    Ok(html.message.detail(title + id))
  }

  def write = compositeAction(Write) { user => implicit template => implicit request =>
    val title = "write message"
    Ok(html.message.write(title))
  }

}
trait AuthConfigImpl extends AuthConfig {

  type ID = String

  type USER = User

  type AUTHORITY = ActionType

  val idManifest = classManifest[ID]

  val sessionTimeoutInSeconds = 3600

  def resolveUser(id: ID) = User.findById(id)

  val loginSucceeded = Redirect(routes.Message.main)

  val logoutSucceeded = Redirect(routes.Application.login)

  val authenticationFailed = Redirect(routes.Application.login)

  val authorizationFailed = Forbidden("no permission")

  def authorize(user: USER, authority: AUTHORITY) = user.permission.executable(authority)

}

trait Base extends Controller with Auth with Pjax with AuthConfigImpl {

  def compositeAction(actionType: ActionType)(f: User => Template => Request[Any] => PlainResult) =
    Action { implicit request =>
      (for {
        user     <- authorized(actionType).right
        template <- pjax(user).right
      } yield f(user)(template)(request)).merge
    }

}

trait Pjax {
  self: Controller =>

  type Template = String => Html => Html
  def pjax(user: User)(implicit request: Request[Any]): Either[PlainResult, Template] = Right {
    if (request.headers.keys("X-PJAX")) html.pjaxTemplate.apply
    else html.fullTemplate.apply(user)
  }

}

trait AuthConfig {

  type ID

  type USER

  type AUTHORITY

  def idManifest: ClassManifest[ID]

  def sessionTimeoutInSeconds: Int

  def resolveUser(id: ID): Option[USER]

  def loginSucceeded: PlainResult

  def logoutSucceeded: PlainResult

  def authenticationFailed: PlainResult

  def authorizationFailed: PlainResult

  def authorize(user: USER, authority: AUTHORITY): Boolean

}

trait LoginLogout {
  self: Controller with AuthConfig =>

  def gotoLoginSucceeded(userId: ID): PlainResult = {
    Cache.getAs[String](userId + ":userId").foreach { old =>
      Cache.set(old + ":sessionId", "", 1)
    }
    val sessionId = generateSessionId()
    Cache.set(sessionId + ":sessionId", userId, sessionTimeoutInSeconds)
    Cache.set(userId + ":userId", sessionId, sessionTimeoutInSeconds)
    loginSucceeded.withSession("sessionId" -> sessionId)
  }

  private def generateSessionId(): String = {
    def isAlphaNum(c: Char) = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
    Stream.continually(random.nextPrintableChar).filter(isAlphaNum).take(64).mkString
  }

  private val random = new Random(new SecureRandom())

  def gotoLogoutSucceeded(request: Request[Any]): PlainResult = {
    for {
      sessionId <- request.session.get("sessionId")
      userId <- Cache.getAs[String](sessionId + ":sessionId")
    } {
      Cache.set(sessionId + ":sessionId", "", 1)
      Cache.set(userId + ":userId", "", 1)
    }
    logoutSucceeded.withNewSession
  }

}

trait Auth {
  self: Controller with AuthConfig =>

  def authorizedAction(authority: AUTHORITY)(f: USER => Request[Any] => PlainResult) =
    Action(req => authorized(authority)(req).right.map(u => f(u)(req)).merge)

  def authorized(authority: AUTHORITY)(implicit request: Request[Any]): Either[PlainResult, USER] = for {
    user <- restoreUser(request).toRight(authenticationFailed).right
    _ <- Either.cond(authorize(user, authority), (), authorizationFailed).right
  } yield user

  private def restoreUser(request: Request[Any]): Option[USER] = for {
    sessionId <- request.session.get("sessionId")
    userId <- Cache.getAs[ID](sessionId + ":sessionId")(current, idManifest)
    user <- resolveUser(userId)
  } yield {
    Cache.set(sessionId + ":sessionId", userId, sessionTimeoutInSeconds)
    Cache.set(userId.toString + ":userId", sessionId, sessionTimeoutInSeconds)
    user
  }

}
