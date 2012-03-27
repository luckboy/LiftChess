package pl.luckboy.liftchess.engine.benchmark
import pl.luckboy.liftchess.engine._

object FirstBenchmark 
{
  def test(bd: Board, n: Int) = {
    val moveStack = new MoveStack(1, 256)
    val t0 = System.currentTimeMillis()
    var i = 0
    var res = 0
    while(i < n) {
      moveStack.generatePseudoLegalMoves(bd)
      res = moveStack.size
      moveStack.popMoves()
      i += 1
    }
    val t1 = System.currentTimeMillis()
    println("result: " + res)
    println("time: " + (t1 - t0)  + "ms")    
  }

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
  
  def main(args: Array[String]): Unit = {
    val n = args(0).toInt
    // r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -
    // legal moves: 48
    val bd = Board(Seq(
        BR, __, __, __, BK, __, __, BR,
        BP, __, BP, BP, BQ, BP, BB, __,
        BB, BN, __, __, BP, BN, BP, __,
        __, __, __, WP, WN, __, __, __,
        __, BP, __, __, WP, __, __, __,
        __, __, WN, __, __, WQ, __, BP,
        WP, WP, WP, WB, WB, WP, WP, WP,
        WR, __, __, __, WK, __, __, WR
        ),
        Side.White, (Castling.AllCastling, Castling.AllCastling), SquareOption.None, 0, 1)
    test(bd, n)
    test(bd, n)
  }
}