// Good Code: Using a case class for immutability and pattern matching for concise logic.
case class Person(name: String, age: Int)

object GoodCodeExample extends App {
  val people = List(Person("Alice", 30), Person("Bob", 25), Person("Charlie", 35))

  val adults = people.filter(_.age >= 18)

  adults match {
    case Nil => println("No adults found.")
    case head :: tail => println(s"Adults found: ${adults.map(_.name).mkString(", ")}")
  }


  // Functional approach for calculating average age.
  val averageAge = people.map(_.age).sum.toDouble / people.size
  println(s"Average age: $averageAge")


  //Using for-comprehension for cleaner code.
  for{
    person <- people
    if person.age > 30
  } yield println(s"${person.name} is older than 30")
}


// Bad Code: Mutable variables, nested loops, and unclear logic.
object BadCodeExample extends App {
  var people = Array(Array("Alice", "30"), Array("Bob", "25"), Array("Charlie", "35"))
  var adults = new Array[Array[String]](people.length)
  var i = 0
  while (i < people.length) {
    if (people(i)(1).toInt >= 18) {
      adults(i) = people(i)
    }
    i += 1
  }

  println("Adults:")
  var j = 0
  while (j < adults.length) {
    if (adults(j) != null) {
      println(adults(j)(0))
    }
    j += 1
  }

  var sum = 0
  var count = 0
  var k = 0
  while (k < people.length) {
    sum += people(k)(1).toInt
    count += 1
    k += 1
  }
  println(s"Average age: ${sum.toDouble / count}")
}
