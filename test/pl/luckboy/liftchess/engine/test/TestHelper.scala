/*******************************************************************************
 * Copyright (C) 2012 Łukasz Szpakowski.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

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
  
    def testBoardWithoutHashKey(bd: Board, args: BoardArgs) = {
    val (aPieces, aSide, aCastling, aEnPassant, aHalfmoveClock, aFullmoveNumber) = args
   
    // foldSidePieces
    val res1 = Seq(Side.White, Side.Black).forall {
      side => 
        Seq(Piece.Pawn, Piece.Knight, Piece.Bishop, Piece.Rook, Piece.Queen, Piece.King).forall {
          piece => 
            val aPs = bd.foldSidePieces(side, piece)(Set[(Int, SidePieceOption)]()) { (_, _) => true } { 
              (ps, sq) => ps + ((sq, bd(sq)): (Int, SidePieceOption))
            }
            val ePs = (0 to 63).filter { sq => aPieces(sq) == SidePieceOption.fromSideAndPiece(side, piece) }.map {
              sq => (sq, aPieces(sq))
            }.toSet
            aPs == ePs
        }
    }
    
    // foldAllSidePieces
    val res2 = Seq(Side.White, Side.Black).forall {
      side => 
        val aPs = bd.foldAllSidePieces(side)(Set[(Int, SidePieceOption)]()) { (_, _) => true } { 
          (ps, sq) => ps + ((sq, bd(sq)): (Int, SidePieceOption))
        }
        val ePs = (0 to 63).filter { sq => aPieces(sq).isSide(side) }.map { sq => (sq, aPieces(sq)) }.toSet
        aPs == ePs
    }
    
    // foldPieces
    val res3 = Seq(Piece.Pawn, Piece.Knight, Piece.Bishop, Piece.Rook, Piece.Queen, Piece.King).forall {
      piece => 
        val aPs = bd.foldPieces(piece)(Set[(Int, SidePieceOption)]()) { (_, _) => true } { 
          (ps, sq) => ps + ((sq, bd(sq)): (Int, SidePieceOption))
        }
        val ePs = (0 to 63).filter { sq => aPieces(sq).isPiece(piece) }.map { sq => (sq, aPieces(sq)) }.toSet
        aPs == ePs
    }
    
    // foldAllPieces
    val res4 = {
      val aPs = bd.foldAllPieces(Set[(Int, SidePieceOption)]()) { (_, _) => true } { 
        (ps, sq) => ps + ((sq, bd(sq)): (Int, SidePieceOption))
      }
      val ePs = (0 to 63).filterNot { sq => aPieces(sq).isNone }.map { sq => (sq, aPieces(sq)) }.toSet
      aPs == ePs
    }
    
    res1 && res2 && res3 && res4 &&
    (0 to 63).forall { sq => bd(sq) == aPieces(sq) } &&
    bd.side == aSide &&
    bd.castling(Side.White) == aCastling._1 &&
    bd.castling(Side.Black) == aCastling._2 &&
    bd.enPassant == aEnPassant &&
    bd.halfmoveClock == aHalfmoveClock &&
    bd.fullmoveNumber == aFullmoveNumber
  }

  implicit def testBoard(bd: Board, args: BoardArgs) =
	testBoardWithoutHashKey(bd, args) && bd.hashKey == newBoardTupled(args).hashKey
}
