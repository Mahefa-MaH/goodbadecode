// Good Code: Using a case class and pattern matching for efficient data handling.
case class User(id: Int, name: String, email: String)

object UserDatabase {
  val users: Seq[User] = Seq(
    User(1, "Alice", "alice@example.com"),
    User(2, "Bob", "bob@example.com"),
    User(3, "Charlie", "charlie@example.com")
  )

  def findUserById(id: Int): Option[User] = users.find(_.id == id)

  def processUser(user: User): String = user match {
    case User(id, name, email) => s"User ID: $id, Name: $name, Email: $email"
    case _ => "Invalid user data"
  }
}


// Bad Code:  Mutable variables and inefficient search.  Avoids functional paradigms.
object UserDatabaseBad {
  var users: Array[User] = Array(
    new User(1, "Alice", "alice@example.com"),
    new User(2, "Bob", "bob@example.com"),
    new User(3, "Charlie", "charlie@example.com")
  )

  def findUserById(id: Int): User = {
    var foundUser: User = null
    for (user <- users) {
      if (user.id == id) {
        foundUser = user
        break
      }
    }
    foundUser
  }
  class User(val id: Int, val name: String, val email: String){
  }
  def processUser(user: User): String = {
      if(user == null) return "Invalid User"
      s"User ID: ${user.id}, Name: ${user.name}, Email: ${user.email}"
  }
}

object Main extends App{
  println(UserDatabase.findUserById(1).getOrElse(User(0,"","")).name)
  println(UserDatabase.processUser(User(1,"John","john@example.com")))
  println(UserDatabaseBad.findUserById(2).name)
  println(UserDatabaseBad.processUser(null))
}
