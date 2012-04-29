package pl.luckboy.liftchess.engine.test
import org.scalacheck._
import org.junit.runner.RunWith
import scala.util.Random
import pl.luckboy.liftchess.engine._

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class GameStateTest  extends Properties("GameState")
{
  import TestHelper._
  
  //
  // Generators.
  //
  
  val boardArgsGen = sideGen.map {
    case Side.White =>
      (
          Seq(BR, __, __, __, BK, __, __, BR,
              BP, __, BP, BP, BQ, BP, BB, __,
              BB, BN, __, __, BP, BN, BP, __,
              __, __, __, WP, WN, __, __, __,
              __, BP, __, __, WP, __, __, __,
              __, __, WN, __, __, WQ, __, BP,
              WP, WP, WP, WB, WB, WP, WP, WP,
              WR, __, __, __, WK, __, __, WR
              ),
          Side.White,
          (Castling.AllCastling, Castling.AllCastling),
          SquareOption.None,
          0, 1
          )
    case Side.Black =>
      (
          Seq(BR, __, __, BQ, __, BR, BK, __,
              BP, WP, __, BP, __, __, BP, BP,
              WQ, __, __, __, __, BN, __, __,
              BB, BB, BP, __, BP, __, __, __,
              WN, BP, __, __, __, __, __, __,
              __, WB, __, __, __, WN, WB, BN,
              BP, WP, WP, WP, __, WP, WP, WP,
              WR, __, __, __, WK, __, __, WR
              ),
          Side.Black,
          (Castling.AllCastling, Castling.NoneCastling),
          SquareOption.None,
          0, 1
          )
  }
  
  //
  // Functions.
  //
  
  def legalMoves(args: BoardArgs, isGood: Boolean) = {
    val mvStack = new MoveStack(1, 4096)
    val bd = newBoardTupled(args)
    ({
      if(isGood) 
        mvStack.generatePseudoLegalGoodMovesWithPopMoves(bd) { (0 until mvStack.size).map(mvStack.move) }
      else
        mvStack.generatePseudoLegalMovesWithPopMoves(bd) { (0 until mvStack.size).map(mvStack.move) }
    }).filter { bd.clone().unsafeFoldSuccessor(_)(false) { (_, _) => true } }.sortBy { _.hashCode }.reverse
  }
  
  //
  // Tests.
  //
    
  Seq(("foldSortedSuccessors", false, (gs: GameState) => gs.foldSortedSuccessors[Seq[Move]] _), 
      ("foldSortedSuccessorsWithoutHashKey", false, (gs: GameState) => gs.foldSortedSuccessorsWithoutHashKey[Seq[Move]] _),
      ("foldSortedGoodSuccessors", true, (gs: GameState) => gs.foldSortedGoodSuccessors[Seq[Move]] _), 
      ("foldSortedGoodSuccessorsWithoutHashKey", true, (gs: GameState) => gs.foldSortedGoodSuccessorsWithoutHashKey[Seq[Move]] _)
      ).foreach {
    case (name, isGood, f) =>  {
      property(name + " should return sorted moves") = Prop.forAllNoShrink(boardArgsGen) {
        args =>
          val eMoves = legalMoves(args, isGood)
          val aMoves = f(GameState.fromBoard(newBoardTupled(args)))(new MoveStack(1, 4096)) { _.hashCode } (Seq[Move]()) { 
            (_, _, _) => true
          } { 
            (_, _, _) => true
          } {
            (moves, _, move) => moves :+ move
          }
          aMoves == eMoves
      }

      property(name + " should return part of sorted moves been divide by first stopping function") = Prop.forAllNoShrink(boardArgsGen, Gen.choose(0, 4096)) {
        (args, n) =>
          val moves = legalMoves(args, isGood)
          val (eMoves, rest) = if(moves.size > 0) moves.span(moves(n % moves.size) ==) else (Seq(), Seq())
          val aMoves = f(GameState.fromBoard(newBoardTupled(args)))(new MoveStack(1, 4096)) { _.hashCode } (Seq[Move]()) { 
            (_, _, move) => rest.head != move
          } { 
            (_, _, _) => true
          } {
            (moves, _, move) => moves :+ move
          }
          aMoves == eMoves
      }

      property(name + " should return part of sorted moves been divide by second stopping function") = Prop.forAllNoShrink(boardArgsGen, Gen.choose(0, 4096)) {
        (args, n) =>
          val moves = legalMoves(args, isGood)
          val (part, rest) = if(moves.size > 0) moves.span(moves(n % moves.size) ==) else (Seq(), Seq())
          val eMoves = rest.headOption.map { part :+ _ }.getOrElse(part)
          val aMoves = f(GameState.fromBoard(newBoardTupled(args)))(new MoveStack(1, 4096)) { _.hashCode } (Seq[Move]()) { 
            (_, _, _) => true
          } { 
            (_, _, move) => rest.head != move
          } {
            (moves, _, move) => moves :+ move
          }
          aMoves == eMoves
      }

      property(name + " should twice return same sorted moves") = Prop.forAllNoShrink(boardArgsGen) {
        args =>
          val eMoves = legalMoves(args, isGood)
          val gs = GameState.fromBoard(newBoardTupled(args))
          
          val Seq(aMoves1, aMoves2) =  (1 to 2).map {
            i => 
              f(gs)(new MoveStack(1, 4096)) { _.hashCode } (Seq[Move]()) { 
                (_, _, _) => true
              } { 
                (_, _, _) => true
              } {
                (moves, _, move) => moves :+ move
              }
          }
          aMoves1 == eMoves && aMoves2 == eMoves
      }
    }
  }

  Seq(
      // *
      (
          "normal board",
          (
              Seq(BR, __, __, __, BK, __, __, BR,
                  BP, __, BP, BP, BQ, BP, BB, __,
                  BB, BN, __, __, BP, BN, BP, __,
                  __, __, __, WP, WN, __, __, __,
                  __, BP, __, __, WP, __, __, __,
                  __, __, WN, __, __, WQ, __, BP,
                  WP, WP, WP, WB, WB, WP, WP, WP,
                  WR, __, __, __, WK, __, __, WR
            	  ),
              Side.White,
              (Castling.AllCastling, Castling.AllCastling),
              SquareOption.None,
              0, 1
              ),
          false, false
          ),
      // checkmate
      (
          "checkmate",
          (
              Seq(BR, BN, BB, __, BK, BB, BN, BR,
                  BP, BP, BP, BP, __, BP, BB, BB,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, BP, __, __, __,
                  __, __, __, __, __, __, WP, BQ,
                  __, __, __, __, __, WP, __, __,
                  WP, WP, WP, WP, WP, __, __, WP,
                  WR, WN, WB, WQ, WK, WB, WN, WR
            	  ),
              Side.White,
              (Castling.AllCastling, Castling.AllCastling),
              SquareOption.None,
              0, 1
              ),
          true, false
          ),
      // stalemate
      (
          "stalemate",
          (
              Seq(__, __, __, __, __, __, __, BK,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, WN, WK, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __
            	  ),
              Side.Black,
              (Castling.NoneCastling, Castling.NoneCastling),
              SquareOption.None,
              0, 1
              ),
          false, true
          ),
      // 50 moves
      (
          "50 move rule",
          (
              Seq(__, __, __, __, __, __, __, BK,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  WK, __, __, __, __, __, __, __
            	  ),
              Side.Black,
              (Castling.NoneCastling, Castling.NoneCastling),
              SquareOption.None,
              100, 1
              ),
          false, true
          ),
      // no 50 moves
      (
          "board before draw by 50 move rule",
          (
              Seq(__, __, __, __, __, __, __, BK,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  WK, __, __, __, __, __, __, __
            	  ),
              Side.Black,
              (Castling.NoneCastling, Castling.NoneCastling),
              SquareOption.None,
              99, 1
              ),
          false, false
          )
      ).foreach {
    case (name, args, isLose, isDraw) => {
      property("isLose should return " + isLose + " for " + name) = Prop.forAllNoShrink(Gen.value(())) {
        _ => GameState.fromBoard(newBoardTupled(args)).isLose(args._2) == isLose
      }

      property("isWin should return " + isLose + " for " + name) = Prop.forAllNoShrink(Gen.value(())) {
        _ => GameState.fromBoard(newBoardTupled(args)).isWin(args._2.opposite) == isLose
      }
      
      property("isDraw should return " + isDraw + " for " + name) = Prop.forAllNoShrink(Gen.value(())) {
        _ => GameState.fromBoard(newBoardTupled(args)).isDraw == isDraw
      }
    }
  }
  
  val repsGen = Gen.oneOf(
      // immediate
      (
          (
              Seq(__, __, __, __, __, __, __, BK,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, BP, __,
                  __, __, __, __, __, __, WP, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, WK
            	  ),
              Side.Black,
              (Castling.NoneCastling, Castling.NoneCastling),
              SquareOption.None,
              0, 1
              ),
          List(
              NormalMove(Piece.King, 7, 6, PieceOption.None),
              NormalMove(Piece.King, 63, 62, PieceOption.None),
              NormalMove(Piece.King, 6, 7, PieceOption.None),
              NormalMove(Piece.King, 62, 63, PieceOption.None),
              NormalMove(Piece.King, 7, 6, PieceOption.None),
              NormalMove(Piece.King, 63, 62, PieceOption.None),
              NormalMove(Piece.King, 6, 7, PieceOption.None),
              NormalMove(Piece.King, 62, 63, PieceOption.None)
              )
          ),
      // no immediate
      (
          (
              Seq(__, __, BN, __, __, __, __, __,
                  __, __, __, __, __, BK, __, __,
                  __, __, __, __, BP, __, __, __,
                  __, __, __, __, WP, BP, __, __,
                  __, __, __, __, __, WP, __, __,
                  __, __, WN, __, __, WK, __, __,
                  __, __, __, __, __, __, __, __,
                  __, __, __, __, __, __, __, __
            	  ),
              Side.White,
              (Castling.AllCastling, Castling.AllCastling),
              SquareOption.None,
              0, 1
              ),
          List(
              NormalMove(Piece.Knight, Square(5, 2), Square(6, 4), PieceOption.None),
              NormalMove(Piece.Knight, Square(0, 2), Square(1, 4), PieceOption.None),
              NormalMove(Piece.Knight, Square(6, 4), Square(4, 3), PieceOption.None),
              NormalMove(Piece.Knight, Square(1, 4), Square(2, 2), PieceOption.None),
              NormalMove(Piece.Knight, Square(4, 3), Square(3, 1), PieceOption.None),
              NormalMove(Piece.Knight, Square(2, 2), Square(1, 0), PieceOption.None),
              NormalMove(Piece.Knight, Square(3, 1), Square(5, 2), PieceOption.None),
              NormalMove(Piece.Knight, Square(1, 0), Square(0, 2), PieceOption.None),
              NormalMove(Piece.King, Square(5, 5), Square(6, 4), PieceOption.None),
              NormalMove(Piece.King, Square(1, 5), Square(0, 4), PieceOption.None),
              NormalMove(Piece.King, Square(6, 4), Square(6, 5), PieceOption.None),
              NormalMove(Piece.King, Square(0, 4), Square(0, 5), PieceOption.None),
              NormalMove(Piece.King, Square(6, 5), Square(5, 5), PieceOption.None),
              NormalMove(Piece.King, Square(0, 5), Square(1, 5), PieceOption.None)
              )
          )
   )
   
  def foldSuccessorPropForReps(mustDraw: Boolean)(fold: (GameState) => (Move) => (Boolean) => ((Boolean, GameState) => Boolean) => Boolean) = {
    Prop.forAllNoShrink(repsGen) {
      case (args, moves) => 
        def g(gs: GameState, moves: List[Move]): Boolean = {
          moves match {
            case Nil           => 
              val res1 = gs.isDraw == mustDraw
              //if(!res1) println("ups " + gs.board.hashKey)
              gs.isLose(gs.board.side) == false && gs.isLose(gs.board.side.opposite) == false && res1
            case move :: moves2 => 
              val res1 = gs.isLose(gs.board.side) == false && gs.isLose(gs.board.side.opposite) == false && gs.isDraw == false
              val res2 = fold(gs)(move)(false) { (_, gs2) => g(gs2, moves2) }
              val res3 = gs.isLose(gs.board.side) == false && gs.isLose(gs.board.side.opposite) == false && gs.isDraw == false
              //if(!(res1 && res3)) println(move, moves2)
              //if(!res2) println(gs.board.hashKey)
              res1 && res2 && res3
          }
        }
        g(GameState.fromBoard(newBoardTupled(args)), moves)
    }
  }
  
  def foldSuccessorForMakeMove(gs: GameState)(move: Move)(z: Boolean)(f: (Boolean, GameState) => Boolean): Boolean = {
    gs.unsafeMakeMove(move).map {
      undo =>
        val y = f(z, gs)
        gs.unsafeUndoMove(undo)
        y
    }.getOrElse(false)
  }
  
  def foldSuccessorForFoldSortedSuccessors(gs: GameState)(move: Move)(z: Boolean)(f: (Boolean, GameState) => Boolean): Boolean = {
    gs.foldSortedSuccessors(new MoveStack(1, 1024)) { _.hashCode } (z) { (_, _, _) => true}  { (_, _, _) => true } {
      (x, gs2, move2) => if(move == move2) f(x, gs2) else x
    }
  }

  def foldSuccessorForFoldSortedSuccessorsWithoutHashKey(gs: GameState)(move: Move)(z: Boolean)(f: (Boolean, GameState) => Boolean): Boolean = {
    gs.foldSortedSuccessorsWithoutHashKey(new MoveStack(1, 1024)) { _.hashCode } (z) { (_, _, _) => true}  { (_, _, _) => true } {
      (x, gs2, move2) => if(move == move2) f(x, gs2) else x
    }
  }

  Seq(("unsafeFoldSortedSuccessor", (gs: GameState) => gs.unsafeFoldSuccessor[Boolean] _),
      ("unsafeMakeMove", foldSuccessorForMakeMove _),
      ("foldSortedSuccessors", foldSuccessorForFoldSortedSuccessors _)
      ).foreach {
    case (name, f) => 
      property("isDraw should check repetition of position for " + name) = foldSuccessorPropForReps(true)(f)
  }

  Seq(("unsafeFoldSortedSuccessorWithoutHashKey", (gs: GameState) => gs.unsafeFoldSuccessorWithoutHashKey[Boolean] _),
      ("foldSortedSuccessorsWithoutHashKey", foldSuccessorForFoldSortedSuccessorsWithoutHashKey _)
      ).foreach {
    case (name, f) => 
      property("isDraw should not check repetition of position for " + name) = foldSuccessorPropForReps(false)(f)
  }
}