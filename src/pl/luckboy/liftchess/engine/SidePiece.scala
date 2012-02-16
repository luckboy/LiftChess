package pl.luckboy.liftchess.engine

/** Klasa Bierki strony.
 * 
 * @author Łukasz Szpakowski
 */
class SidePiece private(val id: Int, val name: String) extends EnumValue

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
      
  def fromSideAndPiece(side: Side, piece: Piece): SidePiece =
    Values(side.id)(piece.id - 1)
    
  def apply(id: Int): SidePiece = 
    Values((id >> 4) - 1)((id & 15) - 1)
}