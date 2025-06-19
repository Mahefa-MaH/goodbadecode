// Good Code: Using a case class and pattern matching for efficient and readable code.
case class User(id: Int, name: String, email: String)

object GoodCodeExample extends App {
  val users = List(User(1, "Alice", "alice@example.com"), User(2, "Bob", "bob@example.com"))

  val alice = users.find(_.id == 1) match {
    case Some(user) => user
    case None => throw new Exception("Alice not found")
  }

  println(s"Alice's email: ${alice.email}")


  //Efficiently filtering and transforming user data.
  val emails = users.filter(_.name.startsWith("A")).map(_.email)
  println(s"Emails starting with A: $emails")
}


// Bad Code:  Inefficient and less readable code using mutable variables and imperative style.
object BadCodeExample extends App {
  var users = List[User]()
  users ::= User(1,"Alice","alice@example.com")
  users ::= User(2,"Bob","bob@example.com")

  var aliceEmail = ""
  for(user <- users){
    if(user.id == 1){
      aliceEmail = user.email
    }
  }
  println(s"Alice's email (bad): $aliceEmail")

  var emailsStartingWithA = List[String]()
  for(user <- users){
      if(user.name.startsWith("A")){
          emailsStartingWithA ::= user.email
      }
  }
  println(s"Emails starting with A (bad): $emailsStartingWithA")
}

