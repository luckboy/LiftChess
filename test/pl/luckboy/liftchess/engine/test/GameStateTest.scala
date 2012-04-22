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
    case (name, isGood, f) =>      
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
  }
}