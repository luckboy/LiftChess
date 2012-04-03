package pl.luckboy.liftchess.engine

/** A singleton for the Zobrist scheme.
 * 
 * @author Łukasz Szpakowski
 */
object Zobrist
{
  /** The key array for pieces and their squares. */
  val pieceSquareKeys: Array[Array[Array[Long]]] = Array.fill(64) {
    Side.makeArray(
        Piece.makeArray(0, 0, 0, 0, 0, 0),
        Piece.makeArray(0, 0, 0, 0, 0, 0)
        )
  }
  
  /** The key array for castling. */
  val castlingKeys: Array[Long] = Castling.makeArray(0, 0, 0, 0)

  /** The key array for en passant. */
  val enPassantKeys: Array[Long] = Array.fill(65)(0)  
  
  /** The key for side. */
  def sideKey(side: Side): Long =
    side.id
}