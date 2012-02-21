package pl.luckboy.liftchess.engine.test
import org.scalacheck._
import org.junit.runner.RunWith
import scala.util.Random
import pl.luckboy.liftchess.engine._

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class BoardTest extends Properties("Board")
{
  //
  // Generatory.
  //
  
  val piecesGen = {
    val wp = Gen.oneOf(
        SidePieceOption.WhitePawn, 
        SidePieceOption.WhiteKnight,
        SidePieceOption.WhiteBishop,
        SidePieceOption.WhiteRook,
        SidePieceOption.WhiteQueen)
    val wn = Gen.value(SidePieceOption.WhiteKnight)
    val wb = Gen.value(SidePieceOption.WhiteBishop)
    val wr = Gen.value(SidePieceOption.WhiteRook)
    val wq = Gen.value(SidePieceOption.WhiteQueen)

    val bp = Gen.oneOf(
        SidePieceOption.BlackPawn, 
        SidePieceOption.BlackKnight,
        SidePieceOption.BlackBishop,
        SidePieceOption.BlackRook,
        SidePieceOption.BlackQueen)
    val bn = Gen.value(SidePieceOption.BlackKnight)
    val bb = Gen.value(SidePieceOption.BlackBishop)
    val br = Gen.value(SidePieceOption.BlackRook)
    val bq = Gen.value(SidePieceOption.BlackQueen)

    val gen  = Gen.someOf(
        br, bn, bb, bq,     bb, bn, br,
        bp, bp, bp, bp, bp, bp, bp, bp,
        wp, wp, wp, wp, wp, wp, wp, wp,
        wr, wn, wb, wq,     wb, wn, wr
        )
    gen.map { 
      ps =>
        val qs = ps :+ SidePieceOption.WhiteKing :+ SidePieceOption.BlackKing
        Random.shuffle(qs ++ Seq.fill(64 - qs.size)(SidePieceOption.None))
    }
  }
  
  val sideGen = Gen.oneOf(Side.White, Side.Black)
   
  val halfmoveClockGen = Gen.choose(0, 99)

  val fullmoveNumberGen = Gen.choose(1, 100)
  
  val boardArgsGen = piecesGen.map6(sideGen, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClockGen, fullmoveNumberGen) { case args => args }

  val pieceGen = Gen.oneOf(Piece.Pawn, Piece.Knight, Piece.Bishop, Piece.Rook, Piece.Queen, Piece.King)  
  
  def newBoardTupled(args: (Seq[SidePieceOption], Side, (Castling, Castling), SquareOption, Int, Int)) =
    (Board.apply _).tupled(args)
  
  //
  // Testy.
  //
    
  property("countSidePieces should return number of pieces") =
    Prop.forAll(boardArgsGen, sideGen, pieceGen) { 
      (args, side, piece) =>
        newBoardTupled(args).countSidePieces(side, piece) == args._1.count { spo => spo == SidePieceOption.fromSideAndPiece(side, piece) }
    }
  
  property("countPieces should return number of pieces") = 
    Prop.forAll(boardArgsGen, pieceGen) {
      (args, piece) =>
        newBoardTupled(args).countPieces(piece) == args._1.count { spo => spo.isPiece(piece) }
    }
  
  property("countAllSidePieces should return number of pieces") = 
    Prop.forAll(boardArgsGen, sideGen) {
      (args, side) =>
        newBoardTupled(args).countAllSidePieces(side) == args._1.count { spo => spo.isSide(side) }
    }
  
  property("countAllPieces should return number of pieces") = 
    Prop.forAll(boardArgsGen) {
      args =>
        newBoardTupled(args).countAllPieces == args._1.count { spo => spo != SidePieceOption.None }
    }
  
  property("foldSidePieces should return square list") = 
    Prop.forAll(boardArgsGen, sideGen, pieceGen) {
      (args, side, piece) =>
        val aSqs = newBoardTupled(args).foldSidePieces(side, piece)(List[Int]()) { (_, _) => true } { (sqs, sq) => sqs :+ sq }
        val eSqs = (0 to 63).filter { args._1(_) == SidePieceOption.fromSideAndPiece(side, piece) }
        aSqs == eSqs
    }

  property("foldSidePieces should return number of pieces") = 
    Prop.forAll(boardArgsGen, sideGen, pieceGen, Gen.choose(0, 64), Gen.choose(0, 64)) {
      (args, side, piece, n, m) =>
        val aSum = newBoardTupled(args).foldSidePieces(side, piece)(0) { (sum, _) => sum < m } { (sum, _) => sum + 1 }
        val eSum = args._1.count { spo => spo == SidePieceOption.fromSideAndPiece(side, piece) }.min(m)
        aSum == eSum
    }

  property("foldPieces should return square list") =
    Prop.forAll(boardArgsGen, pieceGen) {
      (args, piece) =>
        val aSqs = newBoardTupled(args).foldPieces(piece)(List[Int]()) { (_, _) => true } { (sqs, sq) => sqs :+ sq }
        val eSqs = (0 to 63).filter { args._1(_).isPiece(piece) }
        aSqs == eSqs
    }
  
  property("foldPieces should return number of pieces") =
    Prop.forAll(boardArgsGen, pieceGen, Gen.choose(0, 64), Gen.choose(0, 64)) {
      (args, piece, n, m) =>
        val aSum = newBoardTupled(args).foldPieces(piece)(0) { (sum, _) => sum < m } { (sum, _) => sum + 1 }
        val eSum = args._1.count { spo => spo.isPiece(piece) }.min(m)
        aSum == eSum
    }
  
  property("foldAllSidePieces should return square list") = 
    Prop.forAll(boardArgsGen, sideGen) {
      (args, side) =>
        val aSqs = newBoardTupled(args).foldAllSidePieces(side)(List[Int]()) { (_, _) => true } { (sqs, sq) => sqs :+ sq }
        val eSqs = (0 to 63).filter { args._1(_).isSide(side) }
        aSqs == eSqs
    }
  
  property("foldAllSidePieces should return number of pieces") =
    Prop.forAll(boardArgsGen, sideGen, Gen.choose(0, 64), Gen.choose(0, 64)) {
      (args, side, n, m) =>
        val aSum = newBoardTupled(args).foldAllSidePieces(side)(0) { (sum, _) => sum < m } { (sum, _) => sum + 1 }
        val eSum = args._1.count { spo => spo.isSide(side) }.min(m)
        aSum == eSum
    }

  
  property("foldAllPieces should return square list") = 
    Prop.forAll(boardArgsGen) {
      args =>
        val aSqs = newBoardTupled(args).foldAllPieces(List[Int]()) { (_, _) => true } { (sqs, sq) => sqs :+ sq }
        val eSqs = (0 to 63).filter { args._1(_) != SidePieceOption.None }
        aSqs == eSqs
    }

  property("foldAllPieces should return number of pieces") =
    Prop.forAll(boardArgsGen, Gen.choose(0, 64), Gen.choose(0, 64)) {
      (args, n, m) =>
        val aSum = newBoardTupled(args).foldAllPieces(0) { (sum, _) => sum < m } { (sum, _) => sum + 1 }
        val eSum = args._1.count { spo => spo != SidePieceOption.None }.min(m)
        aSum == eSum
    }
  
  property("foldMoveSquares") = Prop.forAll((x: Int) => true)    
}