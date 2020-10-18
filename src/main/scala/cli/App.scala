package cli

import core._

object App {

  def main(args: Array[String]): Unit = {
    println("Welcome to Rhitmomachy!")
    val board = Board(8, 16, Vector.fill(8, 16)(EmptySquare)) placePiece(Coord(1, 2), PSquare(Player.Black, 10))
    val view = View.boardToString(board)
    println(view)

  }

}
