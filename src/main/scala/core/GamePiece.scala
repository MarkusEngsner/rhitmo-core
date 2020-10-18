package core

object Player extends Enumeration {
  type Player = Value
  val White, Black = Value

  def next(p: Player): Player = p match {
    case White => Black
    case Black => White
  }
}

import Player.Player

abstract class GamePiece {
  val player: Player

  // The moves that the piece can make independently of other pieces on the board,
  // that is they don't require line of sight
  val nonBlockableMoves: Vector[Coord]

  // Moves that depend on line of sight
  val blockableMoves: Vector[Coord]

}

case class PTriangle(player: Player, n: Int) extends GamePiece {

  override val nonBlockableMoves: Vector[Coord] =
    Vector(Coord(2, 1), Coord(2, -1), Coord(-2, 1), Coord(-2, -1),
      Coord(1, 2), Coord(1, -2), Coord(-1, 2), Coord(-1, -2))

  override val blockableMoves: Vector[Coord] =
    Vector(Coord(2, 0), Coord(-2, 0), Coord(0, 2), Coord(0, -2))
}

case class PSquare(player: Player, n: Int) extends GamePiece {
  override val nonBlockableMoves: Vector[Coord] =
  Vector(Coord(3, 1), Coord(3, -1), Coord(-3, 1), Coord(-3, -1),
    Coord(1, 3), Coord(1, -3), Coord(-1, 3), Coord(-1, -3))

  override val blockableMoves: Vector[Coord] =
    Vector(Coord(3, 0), Coord(-3, 0), Coord(0, 3), Coord(0, -3))
}

case class PCircle(player: Player, n: Int) extends GamePiece {
  override val nonBlockableMoves: Vector[Coord] = Vector(
    Coord(1, 1), Coord(1, -1), Coord(-1, 1), Coord(-1, -1))

  override val blockableMoves = Vector()
}

case class PPyramid(player: Player, pieces: Vector[GamePiece]) extends GamePiece {
  override val nonBlockableMoves: Vector[Coord] = (pieces flatMap (_.nonBlockableMoves)).distinct

  override val blockableMoves: Vector[Coord] = (pieces flatMap (_.blockableMoves)).distinct

}


