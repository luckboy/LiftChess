package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
object Piece extends Enumeration
{
  val Pawn = Value(1, "P")
  val Knight = Value(2, "N")
  val Bishop = Value(3, "B")
  val Rook = Value(4, "R")
  val Queen = Value(5, "Q")
  val King = Value(6, "K")
}