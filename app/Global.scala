import play.api._

import models._
import anorm._

object Global extends GlobalSettings {

  override def onStart(app: Application) {

    if (User.findAll.isEmpty) {
      Seq(
        User("aaaaaa", "alice@example.com", "secret", "Alice", ReadWrite),
        User("bbbbbb", "bob@example.com", "secret", "Bob", ReadOnly),
        User("cccccc", "chris@example.com", "secret", "Chris", WriteOnly)
      ) foreach User.create
    }

  }

}