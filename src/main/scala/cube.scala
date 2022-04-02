case class Row(_1: Char, _2: Char, _3: Char) {
  override def toString: String = s"${_1} ${_2} ${_3}"
  def serialize: String = s"${_1}${_2}${_3}"

  def reverse: Row = Row(_3, _2, _1)

  def colorMap(m: Map[Char, Char]): Row = Row(m(_1), m(_2), m(_3))
}

case class Face(_1: Row, _2: Row, _3: Row) {
  def center: Char = _2._2

  override def toString: String =
    s"""${_1}
       |${_2}
       |${_3}
       |""".stripMargin

  def serialize: String = s"${_1.serialize}${_2.serialize}${_3.serialize}"

  def rotate180: Face = Face(_3.reverse, _2.reverse, _1.reverse)
  def rotateRight90: Face = Face(Row(_3._1, _2._1, _1._1), Row(_3._2, _2._2, _1._2), Row(_3._3, _2._3, _1._3))
  def rotateLeft90: Face = Face(Row(_1._3, _2._3, _3._3), Row(_1._2, _2._2, _3._2), Row(_1._1, _2._1, _3._1))

  def colorMap(m: Map[Char, Char]): Face = Face(_1.colorMap(m), _2.colorMap(m), _3.colorMap(m))
}

case class Cube(U: Face, L: Face, F: Face, R: Face, B: Face, D: Face) {
  override def toString: String =
    s"""      ${U._1}
       |      ${U._2}
       |      ${U._3}
       |${L._1} ${F._1} ${R._1} ${B._1}
       |${L._2} ${F._2} ${R._2} ${B._2}
       |${L._3} ${F._3} ${R._3} ${B._3}
       |      ${D._1}
       |      ${D._2}
       |      ${D._3}
       |""".stripMargin

  def serialize: String = Seq(U, L, F, R, B, D).map(_.serialize).mkString

  def colorMap(m: Map[Char, Char]): Cube =
    Cube(U.colorMap(m), L.colorMap(m), F.colorMap(m), R.colorMap(m), B.colorMap(m), D.colorMap(m))

  def cornerColors: CornersColoring = CornersColoring(Seq(
    CornerColors(F._1._1, U._3._1, L._1._3), CornerColors(F._1._3, R._1._1, U._3._3),
    CornerColors(F._3._3, D._1._3, R._3._1), CornerColors(F._3._1, L._3._3, D._1._1),
    CornerColors(R._1._3, B._1._1, U._1._3), CornerColors(R._3._3, D._3._3, B._3._1),
    CornerColors(B._1._3, L._1._1, U._1._1), CornerColors(B._3._3, D._3._1, L._3._1)
  ))

  /** Return a canonical representation of this cube. */
  def canonicalize: Cube = {
    /* A cube with [[https://upload.wikimedia.org/wikipedia/commons/9/9d/Rubik%27s_cube_colors.svg the standard coloring]]
     * can be oriented in a canonical fashion by placing the G-R-W corner in the upper left corner with the R facing
     * forward.
     *
     *TODO This isn't canonical, though, since the colors in a standard cube can be swapped in 24 different ways
     * so that another corner would become the G-R-W one.
     */
//    standardizeCornerColors.grwToFront.grwToUpperLeft
    CornersColoring.colorMapsByCorners(cornerColors).map(colorMap).map(_.rCenterToFront.orientStandard).min
  }

  /** Standardize the colors in this cube.
    *
    * There are 30 (= 5×3×2) possible arrangements of the six standard colors, ignoring rotation.
    * Only one of those 30 corresponds to
    * [[https://upload.wikimedia.org/wikipedia/commons/9/9d/Rubik%27s_cube_colors.svg the standard cube coloring]].
    * The standard coloring is implicit in the colors of the corners.
    * This method standardizes the coloring by standardizing that of the corners.
    *
    * @return An equivalent cube whose colors are standardized.
    */
  private def standardizeCornerColors: Cube = CornersColoring.colorMapsByCorners(cornerColors).map(colorMap).min

  /** Bring the GRW corner to the front, with the R facing front. */
  private def grwToFront: Cube = {
    if (Set((F._1._1, U._3._1), (F._1._3, R._1._1), (F._3._3, D._1._3), (F._3._1, L._3._3)).contains(('R','W')))
      this
    else if (Set((U._3._1, L._1._3), (U._3._3, F._1._3), (U._1._3, R._1._3), (U._1._1, B._1._3)).contains(('R','W')))
      rotateDown
    else if (Set((D._1._1, F._3._1), (D._1._3, R._3._1), (D._3._1, L._3._1), (D._3._3, B._3._1)).contains(('R','W')))
      rotateUp
    else if (Set((L._1._1, U._1._1), (L._1._3, F._1._1), (L._3._1, B._3._3), (L._3._3, D._1._1)).contains(('R','W')))
      rotateRight
    else if (Set((R._1._1, U._3._3), (R._1._3, B._1._1), (R._3._1, F._3._3), (R._3._3, D._3._3)).contains(('R','W')))
      rotateLeft
    else
      rotateLeft.rotateLeft
  }

  /** Bring the R center to the front. */
  private def rCenterToFront: Cube =
    if (F.center == 'R') this
    else if (U.center == 'R') rotateDown
    else if (D.center == 'R') rotateUp
    else if (L.center == 'R') rotateRight
    else if (R.center == 'R') rotateLeft
    else rotateLeft.rotateLeft

  private def rotateDown = Cube(U = B.rotate180, L = L.rotateRight90, F = U, R = R.rotateLeft90, B = D.rotate180, D = F)
  private def rotateUp = Cube(U = F, L = L.rotateLeft90, F = D, R = R.rotateRight90, B = U.rotate180, D = B.rotate180)
  private def rotateRight = Cube(U = U.rotateLeft90, L = B, F = L, R = F, B = R, D = D.rotateRight90)
  private def rotateLeft = Cube(U = U.rotateRight90, L = F, F = R, R = B, B = L, D = D.rotateLeft90)

  /** Bring the GRW corner to the upper left.
    * Assumes that the R face of the corner is facing front.
    */
  private def grwToUpperLeft: Cube = ('R','W') match {
    case (F._1._1, U._3._1) => this
    case (F._1._3, R._1._1) => rotateRight90.rotateRight90.rotateRight90
    case (F._3._3, D._1._3) => rotateRight90.rotateRight90
    case _ => rotateRight90
  }

  /** Orient the cube in the standard way.
    * Assumes that the R face of the corner is facing front.
    */
  private def orientStandard: Cube =
    if (U.center == 'W') this
    else if (R.center == 'W') rotateRight90.rotateRight90.rotateRight90
    else if (D.center == 'W') rotateRight90.rotateRight90
    else                      rotateRight90

  private def rotateRight90: Cube =
    Cube(U = L.rotateRight90, L = D.rotateRight90, F = F.rotateRight90, R = U.rotateRight90, B = B.rotateLeft90, D = R.rotateRight90)

  private def standardizeCenterColors: Cube = {
    val m = Map(
      U.center -> 'W',
      L.center -> 'G', F.center -> 'R', R.center -> 'B', B.center -> 'O',
      D.center -> 'Y'
    )
    colorMap(m)
  }
}

object Cube {
  implicit val ordering: Ordering[Cube] = Ordering.by(_.serialize)
}
