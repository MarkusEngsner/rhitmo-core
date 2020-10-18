package core


abstract class Square

case object EmptySquare extends Square

case class NonEmptySquare(piece: GamePiece) extends Square


case class Coord(file: Int, rank: Int) {
  def +(other: Coord): Coord = Coord(file + other.file, rank + other.rank)

  override def toString: String = f"($file; $rank)"

}


case class Board(fileCount: Int = 8, rankCount: Int = 16,
                 placements: Vector[Vector[Square]] = Vector.fill(8, 16)(EmptySquare)) {


  def isValidCoord(c: Coord): Boolean = ((0 until rankCount) contains c.rank) && ((0 until
    fileCount) contains c.file)

  def isEmpty(c: Coord): Boolean = getSquare(c) match {
    case NonEmptySquare(_) => false
    case EmptySquare => true
  }

  def isValidMove(c: Coord): Boolean = isValidCoord(c) && isEmpty(c)


  // v: the translations (ie movement relative to current square)
  // returns: the filtered, relative movements
  private def validMoves(c0: Coord, v: Vector[Coord]): Vector[Coord] =
    v filter ((move: Coord) => isValidMove(c0 + move))


  // c: a coordinate of the form (x, 0) or (0, y)
  // returns: a list of all coordinates between (0, 0) and c
  def pathUntil(c: Coord): Vector[Coord] = c match {
    case Coord(0, file) => (1 until file).map(Coord(0, _)).toVector
    case Coord(rank, 0) => (1 until rank).map(Coord(_, 0)).toVector
  }

  def pathIsEmpty(c0: Coord)(move: Coord): Boolean =
    pathUntil(move) map (c0 + _) forall isEmpty

  def possibleBlockableMoves(c0: Coord, p: GamePiece): Vector[Coord] =
    validMoves(c0, p.blockableMoves) filter pathIsEmpty(c0)

  def possibleNonBlockableMoves(c0: Coord, p: GamePiece): Vector[Coord] =
    validMoves(c0, p.nonBlockableMoves)

  def absolutePossibleMoves(c: Coord): Vector[Coord] = getSquare(c) match {
    case EmptySquare => Vector()
    case NonEmptySquare(p) => (possibleBlockableMoves(c, p) ++
      possibleNonBlockableMoves(c, p)) map (_ + c)
  }

  private def updateSquare(c: Coord, s: Square): Board = {
    val file = placements(c.file)
    val newPlacements = placements.updated(c.file, file.updated(c.rank, s))
    Board(rankCount, fileCount, newPlacements)
  }

  def getSquare(c: Coord): Square = placements(c.file)(c.rank)


  def placePiece(c: Coord, p: GamePiece): Board = updateSquare(c, NonEmptySquare(p))


  def buildBoard(v: Vector[(Coord, GamePiece)]) =
    v.foldLeft(this) { case (b: Board, (c: Coord, p: GamePiece)) => b.placePiece(c, p) }


  def removePiece(c: Coord): Board = updateSquare(c, EmptySquare)


  def movePiece(c0: Coord, c1: Coord): Board = getSquare(c0) match {
    case EmptySquare => this // TODO: change from failing silently
    case NonEmptySquare(p) => removePiece(c0).placePiece(c1, p)
  }

}
