package pl.luckboy.liftchess.engine

/** Klasa bierki strony opcjonalnej.
 * 
 * @author Łukasz Szpakowski
 */
class SidePieceOption private(val id: Int, val name: String) extends EnumValue
{
  /** Sprawdza czy bierka jest pusta. */
  def isEmpty: Boolean =
    id == 0
  
  /** Sprawdza czy bierka jest danej strony.
   * @param side		strona.
   * @return			jeśli jest danej strony to true.
   */
  def isSide(side: Side): Boolean =
    (id >> 4) == side.id + 1
      
  /** Sprawdza czy bierka jest pusta lub danej strony.
   * @param side		strona.
   * @return			jeśli jest pusta lub danej strony to true.
   */
  def isEmptyOrSide(side: Side): Boolean =
    ((id >> 4) & ((side.id + 1) ^ 3)) == 0

  /** Sprawdza czy bierka jest przeciwnej strony niż dana strona.
   * @param side		strona.
   * @return			jeśli jest przeciwnej strony niż dana strona to true.
   */
  def isOpposite(side: Side): Boolean =
    (id >> 4) == ((side.id + 1) ^ 3)

  /** Sprawdza czy bierka jest pusta lub przeciwnej strony niż dana strona.
   * @param side		strona.
   * @return			jeśli jest pusta lub przeciwnej strony niż dana strona to true.
   */
  def isEmptyOrOpposite(side: Side): Boolean =
    ((id >> 4) & (side.id + 1)) == 0
  
  /** Sprawdza czy bierka jest danego typu.
   * @param	piece		typ bierki.
   * @return			jeśli jest danego typu to true.
   */
  def isPiece(piece: Piece): Boolean =
    (id & 15) == piece.id
  
  /** Składa bierkę strony.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldLeft[T](z: T)(f: (T, SidePiece) => T): T =
    if(id != 0) f(z, SidePiece(id)) else z
    
  /** Składa stronę.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldSide[T](z: T)(f: (T, Side) => T): T =
    if((id & 48) == 0) f(z, Side((id >> 4) - 1)) else z
    
  /** Składa tylko bierkę.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldPiece[T](z: T)(f: (T, Piece) => T): T =
    if((id & 15) == 0) f(z, Piece(id & 15)) else z
}

/** Singleton bierki strony opcjonalnej.
 * 
 * @author Łukasz Szpakowski
 */
object SidePieceOption
{
  val None = new SidePieceOption(0, ".")
  
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

  private val Values = Array(
      Array(None),
      Array(None, WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing),
      Array(None, BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing)
      )

  /** Tworzy bierke strony opcjonalna z strony i bierki.
   * @param side		strona.
   * @param piece		bierka.
   * @return			bierka strony.
   */
  def fromSideAndPiece(side: Side, piece: Piece): SidePieceOption =
    Values(side.id + 1)(piece.id)
    
  def apply(id: Int): SidePieceOption =
    Values(id >> 4)(id & 15)
}