// Good Code: Using immutability and functional programming principles for efficient and safe concurrency.
object GoodCode {
  case class User(id: Int, name: String)

  def updateUser(users: List[User], userId: Int, newName: String): List[User] = {
    users.map { user =>
      if (user.id == userId) user.copy(name = newName) else user
    }
  }

  def main(args: Array[String]): Unit = {
    val users = List(User(1, "Alice"), User(2, "Bob"))
    val updatedUsers = updateUser(users, 1, "Alice Updated")
    println(updatedUsers) // Output: List(User(1,Alice Updated), User(2,Bob))
  }
}


// Bad Code: Mutable state and imperative style leading to potential concurrency issues and reduced readability.
object BadCode {
  var users: Array[User] = Array(User(1, "Alice"), User(2, "Bob"))

  def updateUser(userId: Int, newName: String): Unit = {
    for (i <- users.indices) {
      if (users(i).id == userId) {
        users(i) = users(i).copy(name = newName)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    updateUser(1, "Alice Updated")
    println(users.mkString(", ")) //Output: User(1,Alice Updated),User(2,Bob)
  }
}

