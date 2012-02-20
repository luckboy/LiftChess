package pl.luckboy.liftchess.engine

/** Klasa bierki strony.
 * 
 * @author Łukasz Szpakowski
 */
class SidePiece private(val id: Int, val name: String) extends EnumValue
{
  /** Sprawdza czy czy bierka strony jest danej strony.
   * @param side		strona.
   * @return			jeśli jest danej strony to true.
   */
  def isSide(side: Side): Boolean =
    (id >> 4) == side.id + 1

  /** Sprawdza czy dana bierka jest danego typu.
   * @param	piece		typ bierki.
   * @return			jeśli jest danego typu to true.
   */
  def isPiece(piece: Piece): Boolean =
    (id & 15) == piece.id
  
  /** Podaje strone bierki */
  def side: Side =
    Side((id >> 4) - 1)

  /** Podaje typ bierki */ 
  def piece: Piece =
    Piece(id & 15)
}

/** Singleton bierki strony.
 * 
 * @author Łukasz Szpakowski
 */
object SidePiece
{
  val WhitePawn = new SidePiece(Piece.Pawn.id | 16, "P")
  val WhiteKnight = new SidePiece(Piece.Knight.id | 16, "N")
  val WhiteBishop = new SidePiece(Piece.Bishop.id | 16, "B")
  val WhiteRook = new SidePiece(Piece.Rook.id | 16, "R")
  val WhiteQueen = new SidePiece(Piece.Queen.id | 16, "Q")
  val WhiteKing = new SidePiece(Piece.King.id | 16, "K")

  val BlackPawn = new SidePiece(Piece.Pawn.id | 32, "p")
  val BlackKnight = new SidePiece(Piece.Knight.id | 32, "n")
  val BlackBishop = new SidePiece(Piece.Bishop.id | 32, "b")
  val BlackRook = new SidePiece(Piece.Rook.id | 32, "r")
  val BlackQueen = new SidePiece(Piece.Queen.id | 32, "q")
  val BlackKing = new SidePiece(Piece.King.id | 32, "k")
  
  private val Values = Array(
      Array(WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing),
      Array(BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing)
      )
  
  /** Tworzy bierke strony opcjonalna z strony i bierki.
   * @param side		strona.
   * @param piece		bierka.
   * @return			bierka strony.
   */
  def fromSideAndPiece(side: Side, piece: Piece): SidePiece =
    Values(side.id)(piece.id - 1)
   
  /** Tworzy bierkę strony opcjonalnej z identyfikatora.
   * @param id			identyfikator.
   * @return			bierka strony.
   */
  def apply(id: Int): SidePiece = 
    Values((id >> 4) - 1)((id & 15) - 1)
}