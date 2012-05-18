package pl.luckboy.liftchess.engine

/** A singleton for the Zobrist scheme.
 * 
 * @author Łukasz Szpakowski
 */
object Zobrist
{
  /** The key array for the pieces and their squares. */
  private val mPieceSquareKeys: Array[Array[Array[Long]]] = Array.fill(64) {
    Side.makeArray(
        Piece.makeArray(0, 0, 0, 0, 0, 0),
        Piece.makeArray(0, 0, 0, 0, 0, 0)
        )
  }
  
  /** The key array for the castling. */
  private val mCastlingKeys: Array[Array[Long]] = Side.makeArray(
      Castling.makeArray(0, 0, 0, 0),
      Castling.makeArray(0, 0, 0, 0)
      )

  /** The key array for the en passant. */
  private val mEnPassantKeys: Array[Long] = Array.fill(65)(0)  

  /** The key for the piece and the square. 
   * @param sq			the square.
   * @param side		the side.
   * @param piece		the piece.
   * @return 			the hash key for piece and square.
   */
  def pieceSquareKey(sq: Int, side: Side, piece: Piece): Long =
    mPieceSquareKeys(sq)(side.id)(piece.id)
  
  /** The key for the castling. 
   * @param side		the side.
   * @param	castling	the castling.
   * @return			the hash key for castling.
   */
  def castlingKey(side: Side, castling: Castling): Long =
    mCastlingKeys(side.id)(castling.id)
  
  /** The key for the square of the en passant. 
   * @param side		the square of the en passant.
   * @return			the hash key for the square of the en passant.
   */
  def enPassantKey(enPassant: SquareOption): Long =
    mEnPassantKeys(enPassant.id + 1)
  
  /** The key for the side. */
  def sideKey(side: Side): Long =
    side.id

  /** The generator of pseudo random number */
  private var mRandom = new LongRandom(System.currentTimeMillis())
  
  /** Resets the key arrays. 
   * @param r			the generator of pseudo random number.
   */
  def reset(r: LongRandom): Unit = {
    mRandom = r
    (0 to 63).foreach {
      sq => List(Side.White, Side.Black).foreach {
        side => List(Piece.Pawn, Piece.Knight, Piece.Bishop, Piece.Rook, Piece.Queen, Piece.King).foreach {
          piece => mPieceSquareKeys(sq)(side.id)(piece.id) = mRandom.nextLong() << 1
        }
      }
    }
    List(Side.White, Side.Black).foreach {
      side =>
        mCastlingKeys(side.id)(Castling.NoneCastling.id) = 0  
        List(Castling.KingsideCastling, Castling.QueensideCastling, Castling.AllCastling).foreach {
          castling => mCastlingKeys(side.id)(castling.id) = mRandom.nextLong() << 1
        }
    }
    mEnPassantKeys(0) = 0
    (1 to 64).foreach { id => mEnPassantKeys(id) = mRandom.nextLong() << 1 }
  }
  
  reset(mRandom)
}