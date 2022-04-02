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

  /** Standard corners coloring of a Rubik's cube.
    *
    * @see https://upload.wikimedia.org/wikipedia/commons/9/9d/Rubik%27s_cube_colors.svg
    */
  private val standardCornerColors = CornersColoring(Seq(
    CornerColors('G','R','W'), CornerColors('B','W','R'), CornerColors('B','R','Y'), CornerColors('G','Y','R'),
    CornerColors('B','O','W'), CornerColors('O','B','Y'), CornerColors('O','G','W'), CornerColors('O','Y','G')
  ))

  /** Map from a valid corners coloring to the color map that standardizes it. */
  val colorMapByCorners: Map[CornersColoring, Map[Char,Char]] = {
    val colors = Seq('Y','R','G','O','B','W')
    val allColorMaps = colors.permutations.map(_.zip(colors).toMap)

    allColorMaps.map(m => {
      val reverseMap = m.map{ case (k,v) => (v,k) }
      val remappedCorners = CornersColoring(standardCornerColors.corners.map(_.colorMap(reverseMap)))
      remappedCorners -> m
    }).toMap
  }

  /** Map from a valid corners coloring to the color maps that standardize it. */
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
