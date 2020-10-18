package cli

import core._


// Methods for converting board to graphics
object View {


  val gHeight = 5
  val gWidth = 10 + 3

  def emptyRow(b: Board): String = simpleRow(b, " ")

  def simpleRow(b: Board, s: String): String = s.repeat(rowLength(b))

  def rowLength(b: Board): Int = gWidth * b.rankCount

  type Sprite = Vector[String]

  def verticalSepGroup(sampleSq: Sprite): Sprite = {
    val height = sampleSq.length
    Vector.fill(height)(" . ")
  }

  def interleave[T](v: Vector[T], e: T): Vector[T] = {
    v match {
      case Vector(x) => Vector(x)
      case x0 +: xs => x0 +: e +: interleave(xs, e)
    }

  }


  def addVerticalSeparation(squares: Vector[Vector[Sprite]]): Vector[Vector[Sprite]] = {
    val separator = verticalSepGroup(squares.head.head)
    squares map (interleave(_, separator))
  }

  def horizontalSepGroup(sampleRow: Vector[Sprite]): Sprite = {
    val width = sampleRow.foldLeft(0)(_ + _.head.length)
    Vector(".".repeat(width))
  }

  def addHorizontonalSeparation(squares: Vector[Vector[Sprite]])
  : Vector[Vector[Sprite]] = {
    val separator = horizontalSepGroup(squares.head)
    interleave(squares, Vector(separator))
  }

  def concatSprites(s: Vector[Sprite]): Sprite = (for {
    row <- (0 until s.head.length)
  } yield s.foldRight("")((sprite: Sprite, acc: String) => sprite(row) + acc)).toVector

  // merges a 2D vector of sprites into a single large sprite
  def mergeSprites(squares: Vector[Vector[Sprite]]): Sprite = {
    val mergedRows = squares map concatSprites
    mergedRows.reduce(_ ++ _)
  }

  def spriteToString(sp: Sprite): String = sp reduce (_ + "\n" + _)

  def boardToString(b: Board): String = {
    val squares = b.placements map (_ map (squareToString(_)))
    val withVertSep = addVerticalSeparation(squares)
    val withHorizSep = addHorizontonalSeparation(withVertSep)
    val singleSprite = mergeSprites(withHorizSep)
    spriteToString(singleSprite)
  }

  def squareToString(sq: Square): Sprite = sq match {
    case EmptySquare => Vector.fill(5)(" ".repeat(10))
    case NonEmptySquare(p) => pieceToString(p)
  }

  def pieceToString(p: GamePiece): Sprite = p match {
    case PCircle(_, n) => Vector(
      "  .-''-.  ",
      " /      \\ ",
      "|" + n.toString.padTo(8, ' ') + "|",
      " \\      /",
      "  `-..-'  "
    )
    case PSquare(_, n) => Vector(
      "-".repeat(10),
      "|" + " ".repeat(8) + "|",
      "|" + n.toString.padTo(8, ' ') + "|",
      "|" + " ".repeat(8) + "|",
      "-".repeat(10),
    )
    case PTriangle(_, n) => Vector(
      " ".repeat(4) + "/\\" + " ".repeat(4),
      " ".repeat(3) + "/  \\" + " ".repeat(3),
      " ".repeat(2) + "/    \\" + " ".repeat(2),
      " ".repeat(1) + "/ " + n.toString.padTo(5, " ") + "\\" + " ".repeat(1),
      "/" + "_".repeat(8) + "\\"
    )
    case PPyramid(p, _) => Vector(
      "*".repeat(10),
      "*" + " ".repeat(8) + "*",
      "*" + " ".repeat(8) + "*",
      "*" + " ".repeat(8) + "*",
      "*".repeat(10),
    )
  }

}
