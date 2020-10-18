package core


abstract class Square

case object EmptySquare extends Square

case class NonEmptySquare(piece: GamePiece) extends Square


case class Coord(rank: Int, file: Int)

case class Board(fileCount: Int, rankCount: Int, val placements: Vector[Vector[Square]]) {


  def emptyBoard: Board = Board(8, 16, Vector.fill(8, 16)(EmptySquare))

  def isValidCoord(c: Coord): Boolean = ((0 to rankCount) contains c.rank) && ((0 to
    fileCount) contains c.file)


  private def updateSquare(c: Coord, s: Square): Board = {
    val file = placements(c.file)
    val newPlacements = placements.updated(c.file, file.updated(c.rank, s))
    Board(rankCount, fileCount, newPlacements)
  }

  def getSquare(c: Coord): Square = placements(c.file)(c.rank)


  def placePiece(c: Coord, p: GamePiece): Board = updateSquare(c, NonEmptySquare(p))


  def removePiece(c: Coord): Board = updateSquare(c, EmptySquare)


  def movePiece(c0: Coord, c1: Coord): Board = getSquare(c0) match {
    case EmptySquare => this // TODO: change from failing silently
    case NonEmptySquare(p) => removePiece(c0).placePiece(c1, p)
  }

}
