package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
class SidePieceOption private(val id: Int, val name: String) extends EnumValue

/**
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
      Array[SidePieceOption](),
      Array(None, WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing),
      Array(None, BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing)
      )

  def fromSideAndPiece(side: Side, piece: Piece): SidePieceOption =
    Values(side.id + 1)(piece.id)
    
  def apply(id: Int): SidePieceOption =
    Values(id >> 4)(id & 15)
}