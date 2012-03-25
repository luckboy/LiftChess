package pl.luckboy.liftchess.engine

/** Klasa bierki strony opcjonalnej.
 * 
 * @author Łukasz Szpakowski
 */
final class SidePieceOption private(val id: Int, val name: String) extends EnumValue
{
  /** Sprawdza czy bierka strony opcjonalna nie zawiera bierki. */
  @inline
  def isNone: Boolean =
    id == 54
  
  /** Sprawdza czy czy bierka strony opcjonalna jest danej strony.
   * @param side		strona.
   * @return			jeśli jest danej strony to true.
   */
  @inline
  def isSide(side: Side): Boolean =
    (id >> 4) == side.id + 1
      
  /** Sprawdza czy bierka strony opcjonalna nie zawiera bierki lub jest danej strony.
   * @param side		strona.
   * @return			jeśli nie zawiera bierki lub jest danej strony to true.
   */
  @inline
  def isNoneOrSide(side: Side): Boolean =
    ((id >> 4) & (side.id + 1)) != 0
  
  /** Sprawdza czy dana bierka jest danego typu.
   * @param	piece		typ bierki.
   * @return			jeśli jest danego typu to true.
   */
  @inline
  def isPiece(piece: Piece): Boolean =
    (id & 15) == piece.id
  
  /** Składa bierkę strony.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  @inline
  def foldLeft[@specialized T](z: T)(f: (T, SidePiece) => T): T =
    if(id != 54) f(z, SidePiece(id)) else z
    
  /** Składa stronę.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  @inline
  def foldSide[@specialized T](z: T)(f: (T, Side) => T): T =
    if((id & 48) != 48) f(z, Side((id >> 4) - 1)) else z
    
  /** Składa tylko bierkę.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  @inline
  def foldPiece[@specialized T](z: T)(f: (T, Piece) => T): T =
    if((id & 15) != 6) f(z, Piece(id & 15)) else z
}

/** Singleton bierki strony opcjonalnej.
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

  /** Tworzy bierke strony opcjonalna z strony i bierki.
   * @param side		strona.
   * @param piece		bierka.
   * @return			bierka strony.
   */
  def fromSideAndPiece(side: Side, piece: Piece): SidePieceOption =
    Values(side.id)(piece.id)
    
  /** Tworzy bierkę strony opcjonalnej z identyfikatora.
   * @param id			identyfikator.
   * @return			bierka strony.
   */
  def apply(id: Int): SidePieceOption =
    if(id != None.id) Values((id >> 4) - 1)(id & 15) else None
}