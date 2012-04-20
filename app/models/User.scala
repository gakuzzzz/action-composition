package models

import play.api.db._
import anorm._
import anorm.SqlParser._
import play.api.Play.current
import org.apache.commons.codec.digest.DigestUtils._
import java.sql.Clob

case class User(id: String, email: String, password: String, screenName: String, permission: Permission)

object User {

  implicit val rowToPermission: Column[Permission] = {
    Column.nonNull[Permission] { (value, meta) =>
      value match {
        case clob: Clob => clob.getSubString(1, clob.length.toInt) match {
          case "ReadWrite" => Right(ReadWrite)
          case "ReadOnly" => Right(ReadOnly)
          case "WriteOnly" => Right(WriteOnly)
        }
        case _ => Left(TypeDoesNotMatch(
          "Cannot convert %s : %s to Permission for column %s".format(value, value.getClass, meta.column)))
      }
    }
  }

  val simple = {
    get[String]("user.id") ~
    get[String]("user.email") ~
    get[String]("user.password") ~
    get[String]("user.screenName") ~
    get[Permission]("user.permission") map {
      case id~email~pass~name~perm => User(id, email, pass, name, perm)
    }
  }

  def authenticate(email: String, password: String): Option[User] = {
    findByEmail(email).filter { user => user.password == hash(password, user.id) }
  }

  private def hash(pass: String, salt: String): String = sha256Hex(salt.padTo('0', 256) + pass)

  def findByEmail(email: String): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL("SELECT * FROM user WHERE email = {email}").on(
        'email -> email
      ).as(simple.singleOpt)
    }
  }

  def findById(id: String): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL("SELECT * FROM user WHERE id = {id}").on(
        'id -> id
      ).as(simple.singleOpt)
    }
  }

  def findAll: Seq[User] = {
    DB.withConnection { implicit connection =>
      SQL("select * from user").as(simple *)
    }
  }

  def create(user: User) {
    DB.withConnection { implicit connection =>
      SQL("INSERT INTO user VALUES ({id}, {email}, {pass}, {name}, {perm})").on(
        'id -> user.id,
        'email -> user.email,
        'pass -> hash(user.password, user.id),
        'name -> user.screenName,
        'perm -> user.permission.toString
      ).executeUpdate()
    }
  }


}