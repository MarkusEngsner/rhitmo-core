package cli

import core._

import scala.annotation.tailrec

import java.util.Scanner

object App {

  def main(args: Array[String]): Unit = {
    println("Welcome to Rhitmomachy!")
    val pieces = Vector((Coord(1, 2), PSquare(Player.White, 10)), (Coord(0, 13),
      PSquare(Player.Black, 49)))
    val board = Board().buildBoard(pieces)
    val state = State(board)
    loop(state)
  }


  @tailrec
  def loop(state: State): State = {
    println(View.boardToString(state.board))
    val (c0, c1) = getUserInput(state)
    val newState = state.updated(c0, c1)
    loop(newState)
  }

  def getUserInput(state: State): (Coord, Coord) = {
    val selected = getSelected(state)
    println(f"${selected.file} ${selected.rank} selected.")
    val possibleMoves = state.board.absolutePossibleMoves(selected)
    println("These are your possible moves:")
    println(possibleMoves)
    println("please enter which square you want to move to")
    (selected, getMove(state, selected, possibleMoves))
  }

  @tailrec
  def getSelected(state: State): Coord = {
    println("Please select a piece")
    val c = readCoord()
    if (state.board.isValidCoord(c)) state.board.getSquare(c) match {
      case NonEmptySquare(p) =>
        if (p.player == state.turn) c
        else {
          println("That isn't your piece!")
          getSelected(state)
        }
      case EmptySquare =>
        println("That square is empty")
        getSelected(state)
    }
    else {
      println("Invalid coordinate")
      getSelected(state)
    }
  }


  def readCoord(): Coord = {
    val input = io.StdIn.readLine()
    val s = new Scanner(input)
    Coord(s.nextInt(), s.nextInt())
  }

  @tailrec
  def getMove(state: State, c0: Coord, possibleMoves: Vector[Coord]): Coord = {
    val c = readCoord()
    if (possibleMoves contains c) c
    else {
      println("Invalid move, please enter a valid move")
      getMove(state, c0, possibleMoves)
    }
  }

}
