package core

case class State(board: Board = Board(), turn: Player.Player = Player.White){


  def isWon(c: Coord): Boolean = false

  // Assumption: c0 to c1 is a valid move
  def updated(c0: Coord, c1: Coord): State = {
    val newBoard = board.movePiece(c0, c1)
    State(newBoard, Player.next(turn))
  }

}
