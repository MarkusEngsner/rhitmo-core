package core

object Player extends Enumeration {
  type Player = Value
  val White, Black = Value
}

import Player.Player

abstract class GamePiece{
  val p: Player
}

case class PTriangle(p: Player, n: Int) extends GamePiece
case class PSquare(p: Player, n: Int) extends GamePiece
case class PCircle(p: Player, n: Int) extends GamePiece
case class PPyramid(p: Player, pieces: List[GamePiece]) extends GamePiece


