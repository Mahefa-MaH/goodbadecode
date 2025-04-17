// Good Code: Using a case class and pattern matching for efficient data processing.
case class Data(id: Int, value: String)

object GoodCode {
  def processData(data: List[Data]): Map[Int, String] = {
    data.map(d => (d.id, d.value)).toMap
  }

  def main(args: Array[String]): Unit = {
    val dataList = List(Data(1, "one"), Data(2, "two"), Data(3, "three"))
    val result = processData(dataList)
    println(result) // Output: Map(1 -> one, 2 -> two, 3 -> three)

    // Handling potential exceptions gracefully.
    val dataListWithException = List(Data(1, "one"), Data(2, null),Data(3, "three"))
    val resultWithExceptionHandling =  try {
      processData(dataListWithException)
    } catch {
      case e: NullPointerException => println(s"Exception caught: ${e.getMessage}")
      Map.empty[Int,String]
    }
    println(resultWithExceptionHandling)
  }
}


// Bad Code:  Inefficient and error-prone approach.
object BadCode {
  def processData(data: List[(Int, String)]): Map[Int, String] = {
    val map = new scala.collection.mutable.HashMap[Int, String]()
    for (element <- data) {
      map.put(element._1, element._2)
    }
    map.toMap
  }

  def main(args: Array[String]): Unit = {
    val dataList = List((1, "one"), (2, "two"), (3, "three"))
    val result = processData(dataList)
    println(result) //Output: Map(1 -> one, 2 -> two, 3 -> three)
  }
}
