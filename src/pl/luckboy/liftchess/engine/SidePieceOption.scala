package pl.luckboy.liftchess.engine

/** A class for the optional side piece.
 * 
 * @author Łukasz Szpakowski
 */
final class SidePieceOption private(val id: Int, val name: String) extends EnumValue
{
  /** Checks whether optional side piece doesn't contain the piece. */
  @inline
  def isNone: Boolean =
    id == 54

  /** Checks whether the optional side piece is the specified side.
   * @param side		the side.
   * @return			true if it is the specified side.
   */
  def isSide(side: Side): Boolean =
    (id >> 4) == side.id + 1
      
  /** Checks whether the optional side piece doesn't contain the piece or is the specified side.
   * @param side		the side.
   * @return			true if it doesn't contain the piece or it is the specified side.
   */
  def isNoneOrSide(side: Side): Boolean =
    ((id >> 4) & (side.id + 1)) != 0
  
  /** Checks whether optional side piece is the specified piece.
   * @param piece		the piece.
   * @return			true if it is the specified piece.
   */
  def isPiece(piece: Piece): Boolean =
    (id & 15) == piece.id

  /** Returns the optional side. */
  @inline
  def sideOption: SideOption =
    SideOption((id >> 4) - 1)

  /** Returns the optional piece. */
  @inline
  def pieceOption: PieceOption =
    PieceOption(id & 15)

  /** Returns the identifier of the optional side. */
  @inline
  def sideOptionId: Int =
    (id >> 4) - 1

  /** Returns the identifier of the optional piece. */
  @inline
  def pieceOptionId: Int =
    id & 15
    
  /** Folds side piece.
   * @param	z			the start value.
   * @param f			the folding function.
   * @return 			the folding result.
   */
  @inline
  def foldLeft[@specialized T](z: T)(f: (T, SidePiece) => T): T =
    if(id != 54) f(z, SidePiece(id)) else z
    
  /** Folds side.
   * @param	z			the start value.
   * @param f			the folding function.
   * @return 			the folding result.
   */
  @inline
  def foldSide[@specialized T](z: T)(f: (T, Side) => T): T =
    if((id & 48) != 48) f(z, Side((id >> 4) - 1)) else z
    
  /** Folds piece.
   * @param	z			the start value.
   * @param f			the folding function.
   * @return 			the folding result.
   */
  @inline
  def foldPiece[@specialized T](z: T)(f: (T, Piece) => T): T =
    if((id & 15) != 6) f(z, Piece(id & 15)) else z
}

/** A singleton for the optional side piece.
 * 
 * @author Łukasz Szpakowski
 */
object SidePieceOption
{
  val None = new SidePieceOption(54, "_")
  
  val WhitePawn = new SidePieceOption(SidePiece.WhitePawn.id, SidePiece.WhitePawn.name)
  val WhiteKnight = new SidePieceOption(SidePiece.WhiteKnight.id, SidePiece.WhiteKnight.name)
  val WhiteBishop = new SidePieceOption(SidePiece.WhiteBishop.id, SidePiece.WhiteBishop.name)
  val WhiteRook = new SidePieceOption(SidePiece.WhiteRook.id, SidePiece.WhiteRook.name)
  val WhiteQueen = new SidePieceOption(SidePiece.WhiteQueen.id, SidePiece.WhiteQueen.name)
  val WhiteKing = new SidePieceOption(SidePiece.WhiteKing.id, SidePiece.WhiteKing.name)

  val BlackPawn = new SidePieceOption(SidePiece.BlackPawn.id, SidePiece.BlackPawn.name)
  val BlackKnight = new SidePieceOption(SidePiece.BlackKnight.id, SidePiece.BlackKnight.name)
  val BlackBishop = new SidePieceOption(SidePiece.BlackBishop.id, SidePiece.BlackBishop.name)
  val BlackRook = new SidePieceOption(SidePiece.BlackRook.id, SidePiece.BlackRook.name)
  val BlackQueen = new SidePieceOption(SidePiece.BlackQueen.id, SidePiece.BlackQueen.name)
  val BlackKing = new SidePieceOption(SidePiece.BlackKing.id, SidePiece.BlackKing.name)

  private val Values = Side.makeArray(
      Piece.makeArray(WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing),
      Piece.makeArray(BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing)
      )

  /** Creates a optional side piece from the side and the piece.
   * @param side		the side.
   * @param piece		the piece.
   * @return			a optional side piece.
   */
  def fromSideAndPiece(side: Side, piece: Piece): SidePieceOption =
    Values(side.id)(piece.id)

  /** Creates a optional side piece from the specified identifier.
   * @param				the identifier.
   * @return			a optional side piece.
   */
  def apply(id: Int): SidePieceOption =
    if(id != None.id) Values((id >> 4) - 1)(id & 15) else None
    
  /** Return the set of the side pieces. */
  def values: Set[SidePieceOption] =
    Values.flatten.toSet + None
}