package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
object Castling extends Enumeration
{
  val NoneCastling = Value(0, "-")
  val KingsideCastling = Value(1, "K")
  val QueensideCastling = Value(2, "Q")
  val AllCastling = Value(3, "KQ")
}