package pl.luckboy.liftchess.engine

/** A singleton for the Zobrist scheme.
 * 
 * @author Łukasz Szpakowski
 */
object Zobrist
{
  /** The key array for pieces and their squares. */
  private val mPieceSquareKeys: Array[Array[Array[Long]]] = Array.fill(64) {
    Side.makeArray(
        Piece.makeArray(0, 0, 0, 0, 0, 0),
        Piece.makeArray(0, 0, 0, 0, 0, 0)
        )
  }
  
  /** The key array for castling. */
  private val mCastlingKeys: Array[Array[Long]] = Side.makeArray(
      Castling.makeArray(0, 0, 0, 0),
      Castling.makeArray(0, 0, 0, 0)
      )

  /** The key array for en passant. */
  private val mEnPassantKeys: Array[Long] = Array.fill(65)(0)  

  /** The key for piece and square. 
   * @param sq			the square.
   * @param side		the side.
   * @param piece		the piece.
   * @return 			the hash key for piece and square.
   */
  def pieceSquareKey(sq: Int, side: Side, piece: Piece): Long =
    mPieceSquareKeys(sq)(side.id)(piece.id)
  
  /** The key for castling. 
   * @param side		the side.
   * @param	castling	the castling.
   * @return			the hash key for castling.
   */
  def castlingKey(side: Side, castling: Castling): Long =
    mCastlingKeys(side.id)(castling.id)
  
  /** The key for square of en passant. 
   * @param side		the square of en passant.
   * @return			the hash key for square of en passant.
   */
  def enPassantKey(enPassant: SquareOption): Long =
    mEnPassantKeys(enPassant.id + 1)
  
  /** The key for side. */
  def sideKey(side: Side): Long =
    side.id
}