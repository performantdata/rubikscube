object Main {
  def main(args: Array[String]): Unit = {
    import better.files._

    val file = file"rubiks-example.txt"
    Parser.parseFile(file.contentAsString) match {
      case Left(message) => println(message)
      case Right(forms) =>
        val standardForms = Set(forms.map(_.canonicalize): _*)
        println(standardForms.size)
        standardForms.foreach(println)
    }
  }
}
