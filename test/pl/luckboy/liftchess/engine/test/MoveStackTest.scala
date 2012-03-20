package pl.luckboy.liftchess.engine.test
import org.scalacheck._
import org.junit.runner.RunWith
import pl.luckboy.liftchess.engine._

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class MoveStackTest extends Properties("MoveStack")
{
  import TestHelper._

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
  Seq(("promotions", promotionsGen),
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
}