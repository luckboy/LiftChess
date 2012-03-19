package pl.luckboy.liftchess.engine.test
import org.scalacheck._
import org.junit.runner.RunWith
import pl.luckboy.liftchess.engine._

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class MoveStackTest extends Properties("MoveStack")
{
  import TestHelper._

  val castlingsGen = {
    sideGen.flatMap {
      (side) =>
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
  Seq(("castlings", castlingsGen)
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