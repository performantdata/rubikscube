import Cube.colorMapByCorners

case class Row(_1: Char, _2: Char, _3: Char) {
  override def toString: String = s"${_1} ${_2} ${_3}"

  def reverse: Row = Row(_3, _2, _1)

  def colorMap(m: Map[Char, Char]): Row = Row(m(_1), m(_2), m(_3))
}

case class Face(_1: Row, _2: Row, _3: Row) {
  override def toString: String =
    s"""${_1}
       |${_2}
       |${_3}
       |""".stripMargin

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
    standardizeCornerColors.standardizeFront.standardizeRotation
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
  private def standardizeCornerColors: Cube = colorMap(colorMapByCorners(cornerColors))

  /** Bring the GRW corner to the front, with the R facing front. */
  private def standardizeFront: Cube = {
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

  private def rotateDown = Cube(U = B.rotate180, L = L.rotateRight90, F = U, R = R.rotateLeft90, B = D.rotate180, D = F)
  private def rotateUp = Cube(U = F, L = L.rotateLeft90, F = D, R = R.rotateRight90, B = U.rotate180, D = B.rotate180)
  private def rotateRight = Cube(U = U.rotateLeft90, L = B, F = L, R = F, B = R, D = D.rotateRight90)
  private def rotateLeft = Cube(U = U.rotateRight90, L = F, F = R, R = B, B = L, D = D.rotateLeft90)

  /** Bring the GRW corner to the upper left.
    * Assumes that the R face of the corner is facing front.
    */
  private def standardizeRotation: Cube = ('R','W') match {
    case (F._1._1, U._3._1) => this
    case (F._1._3, R._1._1) => rotateRight90.rotateRight90.rotateRight90
    case (F._3._3, D._1._3) => rotateRight90.rotateRight90
    case _ => rotateRight90
  }

  private def rotateRight90: Cube =
    Cube(U = L.rotateRight90, L = D.rotateRight90, F = F.rotateRight90, R = U.rotateRight90, B = B.rotateLeft90, D = R.rotateRight90)
}

object Cube {
  /** Standard corners coloring of a Rubik's cube.
    *
    * @see https://upload.wikimedia.org/wikipedia/commons/9/9d/Rubik%27s_cube_colors.svg
    */
  private val standardCornerColors = CornersColoring(Seq(
    CornerColors('G','R','W'), CornerColors('B','W','R'), CornerColors('B','R','Y'), CornerColors('G','Y','R'),
    CornerColors('B','O','W'), CornerColors('O','B','Y'), CornerColors('O','G','W'), CornerColors('O','Y','G')
  ))

  /** Map from a valid corners coloring to the color map that standardizes it. */
  private val colorMapByCorners: Map[CornersColoring, Map[Char,Char]] = {
    val colors = Seq('Y','R','G','O','B','W')
    val allColorMaps = colors.permutations.map(_.zip(colors).toMap)

    allColorMaps.map(m => {
      val reverseMap = m.map{ case (k,v) => (v,k) }
      val remappedCorners = CornersColoring(standardCornerColors.corners.map(_.colorMap(reverseMap)))
      remappedCorners -> m
    }).toMap
  }

  val colorMapsByCorners: Map[CornersColoring, Seq[Map[Char,Char]]] = {
    val colors = Seq('Y','R','G','O','B','W')
    val allColorMaps = colors.permutations.map(_.zip(colors).toMap)

    allColorMaps.map(m => {
      val reverseMap = m.map{ case (k,v) => (v,k) }
      val remappedCorners = CornersColoring(standardCornerColors.corners.map(_.colorMap(reverseMap)))
      remappedCorners -> m
    }).toSeq.groupMap(_._1)(_._2)
  }
}

/** The colors of a corner, in counterclockwise order, permuted so that the least character comes first. */
case class CornerColors private (_1: Char, _2: Char, _3: Char) {
  def colorMap(m: Map[Char, Char]): CornerColors = CornerColors(m(_1), m(_2), m(_3))
}

object CornerColors {
  implicit val ordering: Ordering[CornerColors] = Ordering[(Char,Char,Char)].on(x => (x._1, x._2, x._3))

  def apply(_1: Char, _2: Char, _3: Char): CornerColors = {
    assert(_1 != _2 && _1 != _3 && _2 != _3)

    if (_1 < _2) {
      if (_1 < _3) new CornerColors(_1, _2, _3)
      else         new CornerColors(_3, _1, _2)
    } else {  // _2 < _1
      if (_2 < _3) new CornerColors(_2, _3, _1)
      else         new CornerColors(_3, _1, _2)
    }
  }
}

case class CornersColoring private(
  _1: CornerColors, _2: CornerColors, _3: CornerColors, _4: CornerColors,
  _5: CornerColors, _6: CornerColors, _7: CornerColors, _8: CornerColors
) {
  def corners: Seq[CornerColors] = Seq(_1, _2, _3, _4, _5, _6, _7, _8)
}

//noinspection ZeroIndexToHead
object CornersColoring {
  def apply(corners: Seq[CornerColors]): CornersColoring = {
    assert(corners.size == 8)

    val cs = corners.sorted
    new CornersColoring(cs(0), cs(1), cs(2), cs(3), cs(4), cs(5), cs(6), cs(7))
  }
}
