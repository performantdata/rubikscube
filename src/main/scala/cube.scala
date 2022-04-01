case class Row(_1: Char, _2: Char, _3: Char) {
  override def toString: String = s"${_1} ${_2} ${_3}"

  def colorMap(m: Map[Char, Char]): Row = Row(m(_1), m(_2), m(_3))
}

case class Face(row1: Row, row2: Row, row3: Row) {
  override def toString: String =
    s"""$row1
       |$row2
       |$row3
       |""".stripMargin

  def colorMap(m: Map[Char, Char]): Face = Face(row1.colorMap(m), row2.colorMap(m), row3.colorMap(m))
}

case class Cube(U: Face, L: Face, F: Face, R: Face, B: Face, D: Face) {
  override def toString: String =
    s"""      ${U.row1}
       |      ${U.row2}
       |      ${U.row3}
       |${L.row1} ${F.row1} ${R.row1} ${B.row1}
       |${L.row2} ${F.row2} ${R.row2} ${B.row2}
       |${L.row3} ${F.row3} ${R.row3} ${B.row3}
       |      ${D.row1}
       |      ${D.row2}
       |      ${D.row3}
       |""".stripMargin

  def colorMap(m: Map[Char, Char]): Cube =
    Cube(U.colorMap(m), L.colorMap(m), F.colorMap(m), R.colorMap(m), B.colorMap(m), D.colorMap(m))

  def canonicalize: Cube = standardizeCenterColors

  private def standardizeCenterColors: Cube = {
    val m = Map(
      U.row2._2 -> 'W',
      L.row2._2 -> 'G', F.row2._2 -> 'R', R.row2._2 -> 'B', B.row2._2 -> 'O',
      D.row2._2 -> 'Y'
    )
    colorMap(m)
  }
}
