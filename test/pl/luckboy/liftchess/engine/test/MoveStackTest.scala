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
import org.junit.runner.RunWith
import scala.util.Random
import pl.luckboy.liftchess.engine._

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class MoveStackTest extends Properties("engine.MoveStack")
{
  import TestHelper._

  val movesGen = {
    sideGen.flatMap {
      side =>
        val ssGen = Gen.listOfN(2, Gen.oneOf(
            SidePieceOption.fromSideAndPiece(side, Piece.Bishop),
            SidePieceOption.fromSideAndPiece(side, Piece.Rook),
            SidePieceOption.fromSideAndPiece(side, Piece.Queen)
            ))
        val osGen = Gen.listOfN(5, Gen.oneOf(
            SidePieceOption.None,
            SidePieceOption.fromSideAndPiece(side.opposite, Piece.Pawn),
            SidePieceOption.fromSideAndPiece(side.opposite, Piece.Knight),
            SidePieceOption.fromSideAndPiece(side.opposite, Piece.Bishop),
            SidePieceOption.fromSideAndPiece(side.opposite, Piece.Rook),
            SidePieceOption.fromSideAndPiece(side.opposite, Piece.Queen)
            ))
        ssGen.map4(osGen, halfmoveClockGen, fullmoveNumberGen) {
          (ss, os, halfmoveClock, fullmoveNumber) =>
            val Seq(s1, s2) = ss
            val Seq(o1, o2, o3, o4, o5) = os
            val pieces = side match {
              case Side.White => 
                Seq(__, __, __, __, BK, __, __, __,
                    __, __, __, o5, __, o1, __, __,
                    __, __, o2, __, __, __, __, __,
                    __, __, __, __, WN, __, __, __,
                    __, s1, __, __, o4, __, s2, __,
                    WP, __, o3, __, WP, __, __, __,
                    __, __, __, WP, __, WP, __, __,
                    __, __, __, __, WK, __, __, __
                    )
              case Side.Black =>
                Seq(__, __, __, __, BK, __, __, __,
                    __, __, __, BP, __, BP, __, __,
                    BP, __, o3, __, BP, __, __, __,
                    __, s1, __, __, o4, __, __, __,
                    __, __, __, __, BN, __, s2, __,
                    __, __, o2, __, __, __, __, __,
                    __, __, __, o4, __, o1, __, __,
                    __, __, __, __, WK, __, __, __
                    )
            }
            val ba = (pieces, side, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock, fullmoveNumber)
            val (moves, captures) = (0 to 63).filter { src => pieces(src).isSide(side) }.foldLeft(Set[Move](), Set[Move]()) {
              case ((moves, captures), src) =>
                pieces(src).foldPiece(moves, captures) {
                  case ((moves, captures), piece) =>
                    Square.foldMoveSquares(src, side, piece)(moves, captures) { (_, dst) => pieces(dst).isNone } {
                      case ((moves, captures), dst) => 
                        (moves + NormalMove(piece, src, dst, PieceOption.None), captures)
                    } {
                      case ((moves, captures), dst) =>
                        if(pieces(dst).isSide(side.opposite))
                          (moves, captures + Capture(piece, src, dst, PieceOption.None))
                        else
                          (moves, captures)
                    }
                }
            }
            (ba, moves ++ captures, captures, (move: Move) => true)
        }
    }
  }
  
  val promotionsGen = {
    val bsGen = Gen.listOfN(3, Gen.oneOf(false, true))
    val spoGen = Gen.oneOf(
        SidePieceOption.WhiteKnight,
        SidePieceOption.WhiteBishop,
        SidePieceOption.WhiteRook,
        SidePieceOption.WhiteQueen,
        SidePieceOption.BlackKnight,
        SidePieceOption.BlackBishop,
        SidePieceOption.BlackRook,
        SidePieceOption.BlackQueen
        )
    val sposGen = Gen.listOfN(3, spoGen)
    Gen.choose(0, 7).map6(bsGen, sposGen, sideGen, halfmoveClockGen, fullmoveNumberGen) {
      (col, bs, spos, side, halfmoveClock, fullmoveNumber) =>
        import scala.collection.mutable.Seq
        val pieces = side match {
          case Side.White =>
            Seq.fill(6 * 8)(SidePieceOption.None) ++
            Seq.fill(2)(SidePieceOption.WhitePawn) ++
            Seq.fill(4)(SidePieceOption.None) ++
            Seq.fill(2)(SidePieceOption.WhitePawn) ++
            Seq(SidePieceOption.WhiteKing) ++
            Seq.fill(6)(SidePieceOption.None) ++
            Seq(SidePieceOption.BlackKing)
          case Side.Black =>
            Seq(SidePieceOption.WhiteKing) ++
            Seq.fill(6)(SidePieceOption.None) ++
            Seq(SidePieceOption.BlackKing) ++
            Seq.fill(2)(SidePieceOption.BlackPawn) ++
            Seq.fill(4)(SidePieceOption.None) ++
            Seq.fill(2)(SidePieceOption.BlackPawn) ++
            Seq.fill(6 * 8)(SidePieceOption.None)
        }
        val row = if(side == Side.White) 1 else 6
        val src = Square(row, col)
        pieces(src) = SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
        val captures = Square.foldPawnCaptureSquares(src, side)((Set[Move](), 0)) { (_, _) => true } {
          case ((captures, i), dst) => 
            if(bs(i) && spos(i).isSide(side.opposite)) {
              pieces(dst) = spos(i)
              (captures ++ Set(Capture(Piece.Pawn, src, dst, PieceOption.Queen), Capture(Piece.Pawn, src, dst, PieceOption.Knight)), i + 1)
            } else {
              (captures, i + 1)
            }
        }._1
        val moves = Square.foldPawnMoveSquares(src, side)((Set[Move](), 2)) { (_, _) => true } {
          case ((moves, i), dst) =>
            if(bs(i)) {
              (moves ++ Set(NormalMove(Piece.Pawn, src, dst, PieceOption.Queen), NormalMove(Piece.Pawn, src, dst, PieceOption.Knight)), i + 1)
            } else {
              pieces(dst) = spos(i)
              (moves, i + 1)
            }
        }._1
        val ba = (pieces.toSeq, side, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock, fullmoveNumber)
        (ba, moves ++ captures, moves ++ captures, (move: Move) => move.promotionPiece != PieceOption.None)
    }
  }
  
  val enPassantsGen = {
    Gen.choose(0, 7).map5(Gen.listOfN(2, Gen.oneOf(false, true)), sideGen, halfmoveClockGen, fullmoveNumberGen) {
      (col, bs, side, halfmoveClock, fullmoveNumber) =>
        import scala.collection.mutable.Seq
        val pieces = (
            Seq.fill(7)(SidePieceOption.None) ++ Seq(SidePieceOption.BlackKing) ++
            Seq.fill(6 * 8)(SidePieceOption.None) ++
            Seq.fill(7)(SidePieceOption.None) ++ Seq(SidePieceOption.WhiteKing)
        	)
        side match {
          case Side.White =>
            pieces(Square(3, col)) = SidePieceOption.BlackPawn
            Square.foldPawnCaptureSquares(Square(2, col), Side.Black)(0) { (_, _) => true } {
              (i, src) => if(bs(i)) pieces(src) = SidePieceOption.WhitePawn; i + 1
            }
          case Side.Black =>
            pieces(Square(4, col)) = SidePieceOption.WhitePawn
            Square.foldPawnCaptureSquares(Square(5, col), Side.White)(0) { (_, _) => true } {
              (i, src) => if(bs(i)) pieces(src) = SidePieceOption.BlackPawn; i + 1
            }
        }
        val (tmpEnPassant, moves) = side match {
          case Side.White =>
            (
                SquareOption(Square(2, col)), 
                Square.foldPawnCaptureSquares(Square(2, col), Side.Black)((Set[Move](), 0)) { (_, _) => true } {
                  case ((moves, i), src) => (if(bs(i)) moves + EnPassant(src, Square(2, col)) else moves, i + 1)
                }._1)
          case Side.Black =>
            (
                SquareOption(Square(5, col)),
                Square.foldPawnCaptureSquares(Square(5, col), Side.White)((Set[Move](), 0)) { (_, _) => true } {
                  case ((moves, i), src) => (if(bs(i)) moves + EnPassant(src, Square(5, col)) else moves, i + 1)
                }._1)
        }
        val enPassant = if(moves.isEmpty) SquareOption.None else tmpEnPassant
        val ba = (pieces.toSeq, side, (Castling.NoneCastling, Castling.NoneCastling), enPassant, halfmoveClock, fullmoveNumber)
        (ba, moves, moves, (move: Move) => move.moveType == MoveType.EnPassant)
    }
  }
  
  val castlingsGen = {
    sideGen.flatMap {
      side =>
        val spoGen = Gen.oneOf(
            SidePieceOption.fromSideAndPiece(side, Piece.Knight),
            SidePieceOption.fromSideAndPiece(side, Piece.Bishop),
            SidePieceOption.fromSideAndPiece(side, Piece.Rook),
            SidePieceOption.fromSideAndPiece(side, Piece.Queen),
            SidePieceOption.fromSideAndPiece(side.opposite, Piece.Knight),
            SidePieceOption.fromSideAndPiece(side.opposite, Piece.Bishop)
            )
        val gen = Gen.choose(0, 5).map4(Gen.choose(0, 3), spoGen, spoGen) { case x => x }
        gen.map5(castlingGen, castlingGen, halfmoveClockGen, fullmoveNumberGen) {
          case ((i, j, spo1, spo2), sCastling, oCastling, halfmoveClock, fullmoveNumber) => {
            val Seq(p1, p2, p3, p4, p5) = (
                (0 to 2).map { k => if(k == i) spo1 else SidePieceOption.None } ++
                (0 to 1).map { l => if(l == j) spo2 else SidePieceOption.None }
                ).toSeq
            val pieces = side match {
              case Side.White => 
                Seq(BR, __, __, __, BK, __, __, BR,
            	    __, __, __, __, __, __, __, __,
            	    __, __, __, __, __, __, __, __,
            	    __, __, __, __, __, __, __, __,
            	    __, __, __, __, __, __, __, __,
            	    __, __, __, __, __, __, __, __,
            	    WP, __, __, __, __, __, __, WP,
            	    WR, p1, p2, p3, WK, p4, p5, WR
            	    )
              case Side.Black =>
                Seq(BR, p1, p2, p3, BK, p4, p5, BR,
                    BP, __, __, __, __, __, __, BP,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    WR, __, __, __, WK, __, __, WR
            	    )
            }
            val castling = side match {
              case Side.White => (sCastling, oCastling)
              case Side.Black => (oCastling, sCastling)
            }
            val ba = (pieces, side, castling, SquareOption.None, halfmoveClock, fullmoveNumber)
            val moves = sCastling match {
              case Castling.NoneCastling      => 
                Set[Move]()
              case Castling.KingsideCastling  => 
                if(j > 1) Set(KingsideCastling()) else Set[Move]()
              case Castling.QueensideCastling => 
                if(i > 2) Set(QueensideCastling()) else Set[Move]()
              case Castling.AllCastling       => 
                (if(j > 1) Set(KingsideCastling()) else Set[Move]()) ++
                (if(i > 2) Set(QueensideCastling()) else Set[Move]())
            }
            (ba, moves, Set[Move](), (move: Move) => move == KingsideCastling() || move == QueensideCastling())
          }
      }
    }
  }  
  
//  Seq[(String, Gen[(BoardArgs, Set[Move], Set[Move])], (Set[Move], Set[Move]) => Boolean)](
  Seq(("normal moves and captures", movesGen),
      ("promotions", promotionsGen),
      ("en passants", enPassantsGen),
      ("castlings", castlingsGen)
      ).foreach {
    case (name, gen) => {
      property("generatePseudoLegalMoves for " + name + " should return moves") = Prop.forAllNoShrink(gen) {
        case (ba, moves, _, f) =>
          val moveStack = new MoveStack(1, 256)
          moveStack.generatePseudoLegalMoves(newBoardTupled(ba))
          (0 until moveStack.size).map(moveStack.move).filter(f).toSet == moves.filter(f)
      }
      
      property("generatePseudoLegalGoodMoves for " + name + " should return good moves") = Prop.forAllNoShrink(gen) {
        case (ba, _, goodMoves, f) =>
          val moveStack = new MoveStack(1, 256)
          moveStack.generatePseudoLegalGoodMoves(newBoardTupled(ba))
          (0 until moveStack.size).map(moveStack.move).filter(f).toSet == goodMoves.filter(f)
      }
      
      property("generatePseudoLegalMovesWithPopMoves for " + name + " should return moves") = Prop.forAllNoShrink(gen) {
        case (ba, moves, _, f) =>
          val moveStack = new MoveStack(1, 256)
          moveStack.generatePseudoLegalMovesWithPopMoves(newBoardTupled(ba)) {
            (0 until moveStack.size).map(moveStack.move).filter(f).toSet == moves.filter(f)
          }
      }
      
      property("generatePseudoLegalGoodMovesWithPopMoves for " + name + " should return good moves") = Prop.forAllNoShrink(gen) {
        case (ba, _, goodMoves, f) =>
          val moveStack = new MoveStack(1, 256)
          moveStack.generatePseudoLegalGoodMovesWithPopMoves(newBoardTupled(ba)) {
            (0 until moveStack.size).map(moveStack.move).filter(f).toSet == goodMoves.filter(f)
          }
      }
    }
  }

  val recMovesGen = {
    Gen.value(()).map {
      _ => {
        // Pierwszy test.
        val pieces1 = Seq(
            BK, __, __, __, __, __, __, __,
            __, __, __, __, __, __, __, __,
            __, __, __, __, __, __, __, __,
            __, __, __, __, __, __, __, __,            
            __, __, __, BR, __, __, WP, __,
            __, __, BP, __, __, __, __, WP,
            __, __, __, __, WN, __, __, __,
            __, __, __, __, __, __, __, WK
            )
        val moves1 = Set(
            // pionki
            NormalMove(Piece.Pawn, Square(4, 6), Square(3, 6), PieceOption.None),
            NormalMove(Piece.Pawn, Square(5, 7), Square(4, 7), PieceOption.None),
            // skoczek
            Capture(Piece.Knight, Square(6, 4), Square(4, 3), PieceOption.None),
            NormalMove(Piece.Knight, Square(6, 4), Square(4, 5), PieceOption.None),
            Capture(Piece.Knight, Square(6, 4), Square(5, 2), PieceOption.None),
            NormalMove(Piece.Knight, Square(6, 4), Square(5, 6), PieceOption.None),
            NormalMove(Piece.Knight, Square(6, 4), Square(7, 2), PieceOption.None),
            NormalMove(Piece.Knight, Square(6, 4), Square(7, 6), PieceOption.None),
            // król            
            NormalMove(Piece.King, Square(7, 7), Square(6, 6), PieceOption.None),
            NormalMove(Piece.King, Square(7, 7), Square(6, 7), PieceOption.None),
            NormalMove(Piece.King, Square(7, 7), Square(7, 6), PieceOption.None)
        )
        val captures1 = Set(
            Capture(Piece.Knight, Square(6, 4), Square(4, 3), PieceOption.None),
            Capture(Piece.Knight, Square(6, 4), Square(5, 2), PieceOption.None)
            )
        val test1 = ((pieces1, Side.White, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, 0, 1), moves1, captures1)
        // Drugi test.
        val pieces2 = Seq(
            __, __, __, __, BK, __, __, __,
            __, __, __, __, __, BP, __, __,
            __, __, BB, __, __, __, BP, __,
            __, __, __, __, __, __, __, __,            
            __, __, __, __, WP, __, __, __,
            __, __, __, __, __, WP, __, __,
            __, __, __, __, __, __, __, __,
            __, __, __, __, WK, __, __, __
            )
        val moves2 = Set(
            // pionki
            NormalMove(Piece.Pawn, Square(1, 5), Square(2, 5), PieceOption.None),
            NormalMove(Piece.Pawn, Square(1, 5), Square(3, 5), PieceOption.None),
            NormalMove(Piece.Pawn, Square(2, 6), Square(3, 6), PieceOption.None),
            // goniec
            NormalMove(Piece.Bishop, Square(2, 2), Square(0, 0), PieceOption.None),
            NormalMove(Piece.Bishop, Square(2, 2), Square(1, 1), PieceOption.None),
            NormalMove(Piece.Bishop, Square(2, 2), Square(1, 3), PieceOption.None),
            NormalMove(Piece.Bishop, Square(2, 2), Square(3, 1), PieceOption.None),            
            NormalMove(Piece.Bishop, Square(2, 2), Square(4, 0), PieceOption.None),
            NormalMove(Piece.Bishop, Square(2, 2), Square(3, 3), PieceOption.None),            
            Capture(Piece.Bishop, Square(2, 2), Square(4, 4), PieceOption.None),
            // król
            NormalMove(Piece.King, Square(0, 4), Square(0, 3), PieceOption.None),
            NormalMove(Piece.King, Square(0, 4), Square(0, 5), PieceOption.None),
            NormalMove(Piece.King, Square(0, 4), Square(1, 3), PieceOption.None),
            NormalMove(Piece.King, Square(0, 4), Square(1, 4), PieceOption.None)
            )
        val captures2 = Set(Capture(Piece.Bishop, Square(2, 2), Square(4, 4), PieceOption.None))
        val test2 = ((pieces2, Side.Black, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, 0, 1), moves2, captures2)
        // Trzeci test.
        val pieces3 = Seq(
            __, __, __, __, BK, __, __, __,
            BR, __, BB, __, __, __, __, __,
            __, BN, __, __, __, __, __, __,
            __, __, __, __, __, __, __, __,            
            __, __, __, WP, __, __, __, __,
            WR, __, WP, __, __, __, __, __,
            __, __, __, __, __, __, __, __,
            __, __, __, __, WK, __, __, WN
            )
        val moves3 = Set(
            // pionki
            NormalMove(Piece.Pawn, Square(5, 2), Square(4, 2), PieceOption.None),
            NormalMove(Piece.Pawn, Square(4, 3), Square(3, 3), PieceOption.None),
            // skoczek
            NormalMove(Piece.Knight, Square(7, 7), Square(5, 6), PieceOption.None),
            NormalMove(Piece.Knight, Square(7, 7), Square(6, 5), PieceOption.None),
            // wieża
            Capture(Piece.Rook, Square(5, 0), Square(1, 0), PieceOption.None),
            NormalMove(Piece.Rook, Square(5, 0), Square(2, 0), PieceOption.None),
            NormalMove(Piece.Rook, Square(5, 0), Square(3, 0), PieceOption.None),
            NormalMove(Piece.Rook, Square(5, 0), Square(4, 0), PieceOption.None),
            NormalMove(Piece.Rook, Square(5, 0), Square(5, 1), PieceOption.None),
            NormalMove(Piece.Rook, Square(5, 0), Square(6, 0), PieceOption.None),
            NormalMove(Piece.Rook, Square(5, 0), Square(7, 0), PieceOption.None),
            // król
            NormalMove(Piece.King, Square(7, 4), Square(6, 3), PieceOption.None),
            NormalMove(Piece.King, Square(7, 4), Square(6, 4), PieceOption.None),
            NormalMove(Piece.King, Square(7, 4), Square(6, 5), PieceOption.None),
            NormalMove(Piece.King, Square(7, 4), Square(7, 3), PieceOption.None),
            NormalMove(Piece.King, Square(7, 4), Square(7, 5), PieceOption.None)
            )
        val captures3 = Set(Capture(Piece.Rook, Square(5, 0), Square(1, 0), PieceOption.None))
        val test3 = ((pieces3, Side.White, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, 0, 1), moves3, captures3)
        
        (Random.shuffle(List(test1, test2, test3)), Random.shuffle(List(test1, test2, test3)))
      }
    }
  }
  
  property("generatePseudoLegalMoves and popMoves should recursively push and pop nested moves") = 
    Prop.forAllNoShrink(recMovesGen) { 
      case (tests1, tests2) => {
        val moveStack = new MoveStack(tests1.size.max(tests2.size), tests1.size.max(tests2.size) * 256)
        Seq(tests1, tests2).forall {
          tests =>
            val res1 = tests.forall {
              case (ba, moves, _) =>
                moveStack.generatePseudoLegalMoves(newBoardTupled(ba))
                (0 until moveStack.size).map(moveStack.move).toSet == moves
            }
            val res2 = tests.reverse.forall {
              case (_, moves, _) =>
                val b = (0 until moveStack.size).map(moveStack.move).toSet == moves
                moveStack.popMoves()
                b
            }
            res1 && res2
        }
      }
    }

  property("generatePseudoLegalGoodMoves and popMoves should recursively push and pop moves for many boards") = 
    Prop.forAllNoShrink(recMovesGen) { 
      case (tests1, tests2) => {
        val moveStack = new MoveStack(tests1.size.max(tests2.size), tests1.size.max(tests2.size) * 256)
        Seq(tests1, tests2).forall {
          tests =>
            val res1 = tests.forall {
              case (ba, _, goodMoves) =>
                moveStack.generatePseudoLegalGoodMoves(newBoardTupled(ba))
                  (0 until moveStack.size).map(moveStack.move).toSet == goodMoves
            }
            val res2 = tests.reverse.forall {
              case (_, _, goodMoves) =>
                val b = (0 until moveStack.size).map(moveStack.move).toSet == goodMoves
                moveStack.popMoves()
                b
            }
            res1 && res2
        }
      }
    }

  property("generatePseudoLegalMovesWithPopMoves should recursively push and pop many moves for many boards") = 
    Prop.forAllNoShrink(recMovesGen) {
      case (tests1, tests2) => {
        val moveStack = new MoveStack(tests1.size.max(tests2.size), tests1.size.max(tests2.size) * 256)
        def g(ts: List[(BoardArgs, Set[Move], Set[Move])]): Boolean =
          ts match {
            case Nil                  => true
            case (ba, moves, _) :: us => 
              moveStack.generatePseudoLegalMovesWithPopMoves(newBoardTupled(ba)) {
                val res1 = (0 until moveStack.size).map(moveStack.move).toSet == moves
                val res2 = g(us)
                val res3 = (0 until moveStack.size).map(moveStack.move).toSet == moves
                res1 && res2 && res3
              }
          }
        val res1 = g(tests1) 
        val res2 = g(tests2)
        res1 && res2
      }
    }
    
  property("generatePseudoLegalGoodMovesWithPopMoves should recursively push and pop many moves") =
    Prop.forAllNoShrink(recMovesGen) {
      case (tests1, tests2) => {
        val moveStack = new MoveStack(tests1.size.max(tests2.size), tests1.size.max(tests2.size) * 256)
        def g(ts: List[(BoardArgs, Set[Move], Set[Move])]): Boolean =
          ts match {
            case Nil                  => true
            case (ba, _, goodMoves) :: us => 
              moveStack.generatePseudoLegalGoodMovesWithPopMoves(newBoardTupled(ba)) {
                val res1 = (0 until moveStack.size).map(moveStack.move).toSet == goodMoves
                val res2 = g(us)
                val res3 = (0 until moveStack.size).map(moveStack.move).toSet == goodMoves
                res1 && res2 && res3
              }
          }
        val res1 = g(tests1) 
        val res2 = g(tests2)
        res1 && res2
      }
    }
  
  property("swap should exchange moves and scores") =
    Prop.forAllNoShrink(movesGen.flatMap { case (ba, moves, _, _) =>  Gen.choose(0, moves.size - 1).map2(Gen.choose(0, moves.size - 1)) { (i, j) => (ba, moves, i, j) } }) {
      case (ba, moves, i, j) =>
        val moveStack = new MoveStack(1, 256)
        moveStack.generatePseudoLegalMoves(newBoardTupled(ba))
        (0 until moves.size).foreach { k => moveStack.setScore(k, k) }
        val tmpMove1 = moveStack.move(i)
        val tmpMove2 = moveStack.move(j)
        val eMove1 = Move(tmpMove1.piece, tmpMove1.source, tmpMove1.destination, tmpMove1.promotionPiece, tmpMove1.moveType)
        val eMove2 = Move(tmpMove2.piece, tmpMove2.source, tmpMove2.destination, tmpMove2.promotionPiece, tmpMove2.moveType)
        val eScore1 = moveStack.score(i)
        val eScore2 = moveStack.score(j)
        moveStack.swap(i, j)
        val aMove2 = moveStack.move(i)
        val aMove1 = moveStack.move(j)
        val aScore2 = moveStack.score(i)
        val aScore1 = moveStack.score(j)
        val aMoves = (0 until moveStack.size).filter { k => k != i && k != j }.map(moveStack.move).toSet
        val eMoves = moves.filter { move => move != eMove1 && move != eMove2 }.toSet
        val eScores = (0 until moveStack.size).filter { k => k != i && k != j }.map(moveStack.score).toSet
        val aScores = (0 until moveStack.size).filter { k => k != i && k != j }.toSet
        moves.size == moveStack.size &&
        aMove1 == eMove1 && aMove2 == eMove2 && aMoves == eMoves &&
        aScore1 == eScore1 && aScore2 == eScore2 && aScores == eScores
    }
}
