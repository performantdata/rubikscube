//noinspection ZeroIndexToHead
object Parser {
  import fastparse._, NoWhitespace._, Parsed.{Failure,Success}

  private def row[_:P] = P(
    CharIn(" ").rep ~
      StringIn("Y", "R", "G", "O", "B", "W").!.map(_(0)).rep(sep = " ", exactly = 3)
        .map(s => Row(s(0), s(1), s(2)))
  )

  private def oneFace[_:P] =
    P((row ~ CharIn(" ").rep ~ "\r\n").rep(exactly = 3).map(f => Face(f(0), f(1), f(2))))

  private def fourFaces[_:P] = P(
    (row ~ row ~ row ~ row ~ CharIn(" ").rep ~ "\r\n").rep(exactly = 3)
      .map(r => (
        Face(r(0)._1, r(1)._1, r(2)._1),
        Face(r(0)._2, r(1)._2, r(2)._2),
        Face(r(0)._3, r(1)._3, r(2)._3),
        Face(r(0)._4, r(1)._4, r(2)._4)
      ))
  )

  private def cube[_:P] = P(
    CharIn("A-Za-z0-9 ").rep(1) ~/ ":" ~ "\r\n" ~/
    (oneFace ~/ fourFaces ~/ oneFace)
      .map(f => Cube(U = f._1, L = f._2._1, F = f._2._2, R = f._2._3, B = f._2._4, D = f._3))
  )

  private def wholeFile[_:P] = P( cube.rep ~ End )

  def parseFile(file: String): Either[String, Seq[Cube]] = parse(file, wholeFile(_)) match {
    case Success(value, _) => Right(value)
    case f: Failure => Left(f.trace().longAggregateMsg)
  }
}
