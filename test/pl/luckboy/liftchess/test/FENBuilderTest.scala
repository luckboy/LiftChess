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

package pl.luckboy.liftchess.test
import org.scalacheck._
import org.junit.runner.RunWith
import scala.util.Random
import pl.luckboy.liftchess._
import pl.luckboy.liftchess.engine._
import pl.luckboy.liftchess.engine.test._

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class FENBuilderTest extends Properties("FENBuilder")
{
  import TestHelper._

  val sidePieceGen = (sideGen.map2(pieceGen) { (side, piece) => SidePiece.fromSideAndPiece(side, piece) })

  val shortFenAndBoardBuilderGen = {
    // row
    val rowGen = for { 
      n <- Gen.choose(0, 7)
      xs <- Gen.listOfN[(Int, SidePiece)](n, Gen.choose(0, 8).map2(sidePieceGen) { case p => p })
    } yield {
      val row = Array.fill(8)(SidePieceOption.None)
      val (j, fenRow, k) = xs.foldLeft(0, "", 0) { 
        case ((j, s, k), (i, sp)) => 
          if(j + i < 8) {
            row(j + i) = sp
            (j + i + 1, s + (if(i > 0) i.toString else "") + sp, j)
          } else {
            (j, s, k)
          }
      }
      (fenRow + (if(8 - j > 0) 8 - j else ""), row.toList)
    }
    // castlingPair
    val castlingPairGen = for(wCastling <- castlingGen; bCastling <- castlingGen) yield {
      (wCastling, bCastling) match {
        case cp@(Castling.NoneCastling, Castling.NoneCastling) => 
          ("-", cp)
        case cp@(Castling.NoneCastling, _)                     => 
          (bCastling.toString.toLowerCase, cp)
        case cp@(_, Castling.NoneCastling)                     => 
          (wCastling.toString.toUpperCase, cp)
        case cp@(_, _)                                         => 
          (wCastling.toString.toUpperCase + bCastling.toString.toLowerCase, cp)
      }
    }
    // FEN and BoardBuilder
    for { 
      (fenRows, rows) <- Gen.listOfN[(String, List[SidePieceOption])](8, rowGen).map { xs => (xs.map { _._1 }, xs.map { _._2 } ) }
      side <- sideGen
      (fenCastling, castlingPair) <- castlingPairGen
      enPassant <- Gen.oneOf(SquareOption.values.toSeq)
    } yield (
        fenRows.mkString("/") + " " + side.toString + " " + fenCastling + " " + enPassant,
        BoardBuilder(rows.flatten, side, castlingPair, enPassant, 0, 1)
        )
  }

  val fenAndBoardBuilderGen = {
	for {
	  (fen, builder) <- shortFenAndBoardBuilderGen
	  halfmoveClock <- halfmoveClockGen
      fullmoveNumber <- fullmoveNumberGen
	} yield (fen + " " + halfmoveClock + " " + fullmoveNumber, builder.updatedHalfmoveClock(halfmoveClock).updatedFullmoveNumber(fullmoveNumber))
  } 
  
  property("apply should return a correct board builder") =
    Prop.forAllNoShrink(fenAndBoardBuilderGen) {
      case (fen, eBuilder) => FENBuilder(fen) == eBuilder
    }
  
  property("apply should return a currect board builder for the short FEN") =
    Prop.forAllNoShrink(shortFenAndBoardBuilderGen) {
      case (fen, eBuilder) => FENBuilder(fen) == eBuilder
    }

  property("toFENString should return a correct FEN") =
    Prop.forAllNoShrink(fenAndBoardBuilderGen) {
      case (fen, builder) => 
        //println(FENBuilder.toFENString(builder))
        FENBuilder.toFENString(builder) == fen
    }
}