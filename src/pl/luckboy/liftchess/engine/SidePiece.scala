package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
object SidePiece extends Enumeration
{
  val Empty = Value(0, "_")

  val WhitePawn = Value(Piece.Pawn.id | 16, "P")
  val WhiteKnight = Value(Piece.Knight.id | 16, "N")
  val WhiteBishop = Value(Piece.Bishop.id | 16, "B")
  val WhiteRook = Value(Piece.Rook.id | 16, "R")
  val WhiteQueen = Value(Piece.Queen.id | 16, "Q")
  val WhiteKing = Value(Piece.King.id | 16, "K")

  val BlackPawn = Value(Piece.Pawn.id | 32, "p")
  val BlackKnight = Value(Piece.Knight.id | 32, "n")
  val BlackBishop = Value(Piece.Bishop.id | 32, "b")
  val BlackRook = Value(Piece.Rook.id | 32, "r")
  val BlackQueen = Value(Piece.Queen.id | 32, "q")
  val BlackKing = Value(Piece.King.id | 32, "k")
}