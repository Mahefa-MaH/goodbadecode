// Good Code: Using immutable data structures and functional programming principles.

object GoodCode {
  def processData(data: List[Int]): List[Int] = {
    data.filter(_ > 0).map(_ * 2).sorted
  }

  def main(args: Array[String]): Unit = {
    val data = List(-1, 2, 0, 4, -3, 6)
    val processedData = processData(data)
    println(processedData) // Output: List(4, 8, 12)
  }
}


// Bad Code: Mutable variables, imperative style, and less readable.

object BadCode {
  def processData(data: List[Int]): List[Int] = {
    val result = scala.collection.mutable.ListBuffer.empty[Int]
    var i = 0
    while (i < data.length) {
      if (data(i) > 0) {
        result += data(i) * 2
      }
      i += 1
    }
    result.sorted.toList
  }

  def main(args: Array[String]): Unit = {
      val data = List(-1, 2, 0, 4, -3, 6)
      val processedData = processData(data)
      println(processedData) // Output: List(4, 8, 12)
  }
}
