// Good Code: Using immutable data structures and functional programming principles for efficient and predictable behavior.

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object GoodCodeExample extends App {

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  case class User(id: Int, name: String)

  val users: Seq[User] = Seq(User(1, "Alice"), User(2, "Bob"), User(3, "Charlie"))

  def findUserById(userId: Int): Future[Option[User]] = Future {
    users.find(_.id == userId)
  }


  val futureUser: Future[Option[User]] = findUserById(2)

  futureUser.onComplete {
    case Success(user) => println(s"Found user: $user")
    case Failure(exception) => println(s"Error: ${exception.getMessage}")
  }

  Thread.sleep(1000) // allow time for future to complete.  Not ideal, but for demo.
}



// Bad Code: Mutable state, side effects, and lack of error handling lead to unpredictable and hard-to-maintain code.


object BadCodeExample extends App {

  var users: Array[User] = Array(User(1,"Alice"), User(2,"Bob"), User(3, "Charlie"))

  def findUserById(userId: Int): Option[User] = {
    var foundUser: Option[User] = None
    for(user <- users){
      if(user.id == userId){
        foundUser = Some(user)
      }
    }
    foundUser
  }


  val user = findUserById(2)
  println(user)


  users(0) = User(1, "Updated Alice") // Mutable state, dangerous side effects

  println(findUserById(1))

}

case class User(id:Int, name:String)
