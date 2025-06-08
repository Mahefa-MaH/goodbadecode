// Good Code: Using immutable data structures and functional programming principles for better readability and maintainability.

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object GoodCodeExample extends App {

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  case class User(id: Int, name: String)

  val users: Seq[User] = Seq(User(1, "Alice"), User(2, "Bob"), User(3, "Charlie"))

  def findUserById(userId: Int): Future[Option[User]] = Future {
    users.find(_.id == userId)
  }

  def handleUser(user: Option[User]): Unit = {
    user match {
      case Some(u) => println(s"Found user: ${u.name}")
      case None => println("User not found")
    }
  }


  findUserById(2).onComplete {
    case Success(user) => handleUser(user)
    case Failure(exception) => println(s"Error: ${exception.getMessage}")
  }

  Thread.sleep(1000) //wait for future to complete

}


// Bad Code: Mutable state, imperative style, and lack of error handling make this code harder to understand and maintain.

object BadCodeExample extends App {

  var users: Array[User] = Array(User(1,"Alice"), User(2,"Bob"), User(3,"Charlie"))

  def findUserById(userId:Int):Option[User]={
    var foundUser:Option[User] = None
    for(user <- users){
      if(user.id == userId){
        foundUser = Some(user)
      }
    }
    foundUser
  }


  val user = findUserById(2)
  if(user.isDefined){
    println(s"Found user: ${user.get.name}")
  }else{
    println("User not found")
  }
}
