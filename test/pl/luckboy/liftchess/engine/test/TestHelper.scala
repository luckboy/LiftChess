package pl.luckboy.liftchess.engine.test
import org.scalacheck._
import pl.luckboy.liftchess.engine._

object TestHelper
{
  type BoardArgs = (Seq[SidePieceOption], Side, (Castling, Castling), SquareOption, Int, Int)

  val pieceGen = Gen.oneOf(Piece.Pawn, Piece.Knight, Piece.Bishop, Piece.Rook, Piece.Queen, Piece.King)  
  
  val squareGen = Gen.choose(0, 63) 
  
  val sideGen = Gen.value(Side.White) | Gen.value(Side.Black)

  val castlingGen = Gen.oneOf(
      Castling.NoneCastling,
      Castling.KingsideCastling,
      Castling.QueensideCastling,
      Castling.AllCastling)
       
  val halfmoveClockGen = Gen.choose(0, 99)

  val fullmoveNumberGen = Gen.choose(1, 100)

  def newBoardTupled(args: BoardArgs) =
    (Board.apply _).tupled(args)
  
  val __ = SidePieceOption.None

  val WP = SidePieceOption.WhitePawn
  val WN = SidePieceOption.WhiteKnight
  val WB = SidePieceOption.WhiteBishop
  val WR = SidePieceOption.WhiteRook
  val WQ = SidePieceOption.WhiteQueen
  val WK = SidePieceOption.WhiteKing

  val BP = SidePieceOption.BlackPawn
  val BN = SidePieceOption.BlackKnight
  val BB = SidePieceOption.BlackBishop
  val BR = SidePieceOption.BlackRook
  val BQ = SidePieceOption.BlackQueen
  val BK = SidePieceOption.BlackKing

  def newCapture(piece: Piece, src: Int, dst: Int, promPiece: PieceOption) = 
    Capture(piece, src, dst, promPiece)
    
  def newMoveOrCapture(piece: Piece, src: Int, dst: Int, promPiece: PieceOption, args: BoardArgs) = {
    args._1(dst) match {
      case SidePieceOption.None => NormalMove(piece, src, dst, promPiece)
      case _                    => Capture(piece,src, dst, promPiece)
    }
  }
}