package pl.luckboy.liftchess.engine

/** Klasa ruchu.
 * 
 * @author Łukasz Szpakowski
 */
case class Move(piece: Piece, source: Int, destination: Int, promotionPiece: PieceOption, flags: Int)

/**
 * @author Łukasz Szpakowski
 */
object Move
{
  val CaptureFlag = 1
  val KingsideCastleFlag = 2
  val QueensideCastleFlag = 4
}