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
  
  type BoardArgs = (Seq[SidePieceOption], Side, (Castling, Castling), SquareOption, Int, Int)
  
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
    Prop.forAllNoShrink(boardArgsGen, sideGen, pieceGen) { 
      (args, side, piece) =>
        newBoardTupled(args).countSidePieces(side, piece) == args._1.count { spo => spo == SidePieceOption.fromSideAndPiece(side, piece) }
    }
  
  property("countPieces should return number of pieces") = 
    Prop.forAllNoShrink(boardArgsGen, pieceGen) {
      (args, piece) =>
        newBoardTupled(args).countPieces(piece) == args._1.count { spo => spo.isPiece(piece) }
    }
  
  property("countAllSidePieces should return number of pieces") = 
    Prop.forAllNoShrink(boardArgsGen, sideGen) {
      (args, side) =>
        newBoardTupled(args).countAllSidePieces(side) == args._1.count { spo => spo.isSide(side) }
    }
  
  property("countAllPieces should return number of pieces") = 
    Prop.forAllNoShrink(boardArgsGen) {
      args =>
        newBoardTupled(args).countAllPieces == args._1.count { spo => spo != SidePieceOption.None }
    }
  
  property("foldSidePieces should return square set") = 
    Prop.forAllNoShrink(boardArgsGen, sideGen, pieceGen) {
      (args, side, piece) =>
        val aSqs = newBoardTupled(args).foldSidePieces(side, piece)(Set[Int]()) { (_, _) => true } { (sqs, sq) => sqs + sq }
        val eSqs = (0 to 63).filter { args._1(_) == SidePieceOption.fromSideAndPiece(side, piece) }.toSet
        aSqs == eSqs
    }

  property("foldSidePieces should return number of pieces") = 
    Prop.forAllNoShrink(boardArgsGen, sideGen, pieceGen, Gen.choose(0, 64), Gen.choose(0, 64)) {
      (args, side, piece, n, m) =>
        val aSum = newBoardTupled(args).foldSidePieces(side, piece)(0) { (sum, _) => sum < m } { (sum, _) => sum + 1 }
        val eSum = args._1.count { spo => spo == SidePieceOption.fromSideAndPiece(side, piece) }.min(m)
        aSum == eSum
    }

  property("foldPieces should return square set") =
    Prop.forAllNoShrink(boardArgsGen, pieceGen) {
      (args, piece) =>
        val aSqs = newBoardTupled(args).foldPieces(piece)(Set[Int]()) { (_, _) => true } { (sqs, sq) => sqs + sq }
        val eSqs = (0 to 63).filter { args._1(_).isPiece(piece) }.toSet
        aSqs == eSqs
    }
  
  property("foldPieces should return number of pieces") =
    Prop.forAllNoShrink(boardArgsGen, pieceGen, Gen.choose(0, 64), Gen.choose(0, 64)) {
      (args, piece, n, m) =>
        val aSum = newBoardTupled(args).foldPieces(piece)(0) { (sum, _) => sum < m } { (sum, _) => sum + 1 }
        val eSum = args._1.count { spo => spo.isPiece(piece) }.min(m)
        aSum == eSum
    }
  
  property("foldAllSidePieces should return square set") = 
    Prop.forAllNoShrink(boardArgsGen, sideGen) {
      (args, side) =>
        val aSqs = newBoardTupled(args).foldAllSidePieces(side)(Set[Int]()) { (_, _) => true } { (sqs, sq) => sqs + sq }
        val eSqs = (0 to 63).filter { args._1(_).isSide(side) }.toSet
        aSqs == eSqs
    }
  
  property("foldAllSidePieces should return number of pieces") =
    Prop.forAllNoShrink(boardArgsGen, sideGen, Gen.choose(0, 64), Gen.choose(0, 64)) {
      (args, side, n, m) =>
        val aSum = newBoardTupled(args).foldAllSidePieces(side)(0) { (sum, _) => sum < m } { (sum, _) => sum + 1 }
        val eSum = args._1.count { spo => spo.isSide(side) }.min(m)
        aSum == eSum
    }
  
  property("foldAllPieces should return square set") = 
    Prop.forAllNoShrink(boardArgsGen) {
      args =>
        val aSqs = newBoardTupled(args).foldAllPieces(Set[Int]()) { (_, _) => true } { (sqs, sq) => sqs + sq }
        val eSqs = (0 to 63).filter { args._1(_) != SidePieceOption.None }.toSet
        aSqs == eSqs
    }

  property("foldAllPieces should return number of pieces") =
    Prop.forAllNoShrink(boardArgsGen, Gen.choose(0, 64), Gen.choose(0, 64)) {
      (args, n, m) =>
        val aSum = newBoardTupled(args).foldAllPieces(0) { (sum, _) => sum < m } { (sum, _) => sum + 1 }
        val eSum = args._1.count { spo => spo != SidePieceOption.None }.min(m)
        aSum == eSum
    }
  
  //
  // Generatorów ataków i szachów.
  //
  
  def pawnAttacks(spos: Seq[SidePieceOption], side: Side) = {
    val __ = SidePieceOption.None
    val Seq(k1, p1, p2, a1, a2) = spos
    val pieces = side match {
      case Side.White =>
        Seq(__, __, __, __, p1, __, __, __,
        	__, __, __, __, __, __, __, __,
        	__, __, __, __, __, __, __, __,
        	__, __, __, __, __, __, __, __,
        	__, __, __, k1, __, __, __, __,
        	__, __, a1, __, a2, __, __, __,
        	__, __, __, __, __, __, __, __,
        	__, __, __, __, p2, __, __, __
            )
      case Side.Black =>
        Seq(__, __, __, __, p1, __, __, __,
        	__, __, __, __, __, __, __, __,
        	__, __, __, __, __, __, __, __,
        	__, __, a1, __, a2, __, __, __,
        	__, __, __, k1, __, __, __, __,
        	__, __, __, __, __, __, __, __,
        	__, __, __, __, __, __, __, __,
        	__, __, __, __, p2, __, __, __
        	)
    }
    val res = Seq(a1, a2).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.Pawn) }
     
    (pieces, Square(4, 3), side, res)
  }

  def knightAttacks(spos: Seq[SidePieceOption], side: Side) = {
    val __ = SidePieceOption.None
    val Seq(k1, p1, p2, a1, a2, a3, a4, a5, a6, a7, a8) = spos
    val pieces = Seq(
        __, __, __, __, p1, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, a1, __, a2, __, __, __,
        __, a3, __, __, __, a4, __, __,
        __, __, __, k1, __, __, __, __,
        __, a5, __, __, __, a6, __, __,
        __, __, a7, __, a8, __, __, __,
        __, __, __, __, p2, __, __, __
        )
     val res = Seq(a1, a2, a3, a4, a5, a6, a7, a8).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.Knight) }
     
     (pieces, Square(4, 3), side, res)
  }
  
  def kingAttacks(spos: Seq[SidePieceOption], side: Side) = {
    val __ = SidePieceOption.None
    val Seq(k1, p1, p2, a1, a2, a3, a4, a5, a6, a7, a8) = spos
    val pieces = Seq(
        __, __, __, p1, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, a1, a2, a3, __, __,
        __, __, __, a4, k1, a5, __, __,
        __, __, __, a6, a7, a8, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, p2, __, __, __, __
        )
    val res = (
        (Seq(a1, a3).exists { _ == SidePieceOption.BlackPawn } && side == Side.Black) ||
        (Seq(a6, a8).exists { _ == SidePieceOption.WhitePawn } && side == Side.White) ||
        Seq(a1, a3, a6, a8).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.Bishop) } ||
        Seq(a2, a4, a5, a7).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.Rook) } ||
        Seq(a1, a2, a3, a4, a5, a6, a7, a8).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.Queen) } ||
        Seq(a1, a2, a3, a4, a5, a6, a7, a8).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.King) }
        )
    
    (pieces, Square(3, 4), side, res)
  }

  def queenAttacks(spos: Seq[SidePieceOption], side: Side) = {
    val __ = SidePieceOption.None
    val Seq(k1, p1, p2, a1, a2, a3, a4, a5, a6, a7, a8) = spos
    val pieces = Seq(
        __, __, __, a2, p1, __, __, a3,
        a1, __, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        a4, __, __, k1, __, __, a5, __,
        __, __, __, __, __, __, __, __,
        __, __, __, a7, __, a8, __, __,
        a6, __, __, __, p2, __, __, __
        )
     val res = (
         Seq(a1, a3, a6, a8).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.Bishop) } ||
         Seq(a2, a4, a5, a7).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.Rook) } ||
         Seq(a1, a2, a3, a4, a5, a6, a7, a8).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.Queen) }
         )
     
     (pieces, Square(4, 3), side, res)
  }

  def bishopAttacks(spos: Seq[SidePieceOption], side: Side) = {
    val __ = SidePieceOption.None
    val Seq(k1, p1, p2, a1, a2, a3, a4) = spos
    val pieces = Seq(
        __, __, __, __, p1, __, __, __,
        __, k1, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, a1, __, __, __, __,
        __, __, __, __, a2, __, __, __,
        __, __, __, __, __, a3, __, __,
        __, __, __, __, __, __, a4, __,
        __, __, __, __, p2, __, __, __
        )
     val res = (
         Seq(a1, a2, a3, a4).filterNot { _ == SidePieceOption.None }.headOption.map { 
           spo => 
             spo == SidePieceOption.fromSideAndPiece(side, Piece.Bishop) ||
             spo == SidePieceOption.fromSideAndPiece(side, Piece.Queen)
         }.getOrElse(false)
         )
     
     (pieces, Square(1, 1), side, res)
  }

  
  def rookAttacks(spos: Seq[SidePieceOption], side: Side) = {
    val __ = SidePieceOption.None
    val Seq(k1, p1, p2, a1, a2, a3, a4) = spos
    val pieces = Seq(
        __, __, __, __, p1, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, a4, a3, a2, a1, __, k1, __,
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, __, p2, __, __, __
        )
     val res = (
         Seq(a1, a2, a3, a4).filterNot { _ == SidePieceOption.None }.headOption.map { 
           spo => 
             spo == SidePieceOption.fromSideAndPiece(side, Piece.Rook) ||
             spo == SidePieceOption.fromSideAndPiece(side, Piece.Queen)
         }.getOrElse(false)
         )
     
     (pieces, Square(4, 6), side, res)
  }
  
  val sidePieceOptionGenWithoutKing = Gen.oneOf(
      SidePieceOption.None,
      SidePieceOption.WhitePawn,
      SidePieceOption.WhiteKnight,
      SidePieceOption.WhiteBishop,
      SidePieceOption.WhiteRook,
      SidePieceOption.WhiteQueen,
      SidePieceOption.BlackPawn,
      SidePieceOption.BlackKnight,
      SidePieceOption.BlackBishop,
      SidePieceOption.BlackRook,
      SidePieceOption.BlackQueen) 
  
  def attackGen(n: Int, f: (Seq[SidePieceOption], Side) => (Seq[SidePieceOption], Int, Side, Boolean)) = {
    val sposGen = Gen.listOfN(n, sidePieceOptionGenWithoutKing).map { 
      spos => 
        val newSpos = List(SidePieceOption.WhiteKing, SidePieceOption.BlackKing) ++ spos :+ SidePieceOption.None
        Random.shuffle(newSpos)
    }
    
    sideGen.flatMap {
      aSide =>
        sposGen.map6(sideGen, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClockGen, fullmoveNumberGen) {
         (spos, side, castling, enPassant, halfmoveClock, fullmoveNumber) =>
           val (pieces, sq, rSide, res) = f(spos, aSide)
           
           ((pieces, side, castling, enPassant, halfmoveClock, fullmoveNumber), sq, rSide, res)
        }
    }
  }
  
  def sideInCheckGen(n: Int, f: (Seq[SidePieceOption], Side) => (Seq[SidePieceOption], Int, Side, Boolean)) = {
    val sposGen = Gen.listOfN(n, sidePieceOptionGenWithoutKing)

    sideGen.flatMap {
      aSide =>
        sposGen.map6(sideGen, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClockGen, fullmoveNumberGen) {
         (spos, side, castling, enPassant, halfmoveClock, fullmoveNumber) =>
           val newSpos = spos :+ SidePieceOption.fromSideAndPiece(aSide.opposite, Piece.King) :+ SidePieceOption.None
           val shuffledSpos = Random.shuffle(newSpos)
           val (pieces, sq, rSide, res) = f(SidePieceOption.fromSideAndPiece(aSide, Piece.King) :: spos, aSide)
           
           ((pieces, side, castling, enPassant, halfmoveClock, fullmoveNumber), sq, rSide, res)
        }
    }
  }
  
  def inCheckGen(n: Int, f: (Seq[SidePieceOption], Side) => (Seq[SidePieceOption], Int, Side, Boolean)) = {
    val sposGen = Gen.listOfN(n, sidePieceOptionGenWithoutKing)

    sposGen.map6(sideGen, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClockGen, fullmoveNumberGen) {
      (spos, side, castling, enPassant, halfmoveClock, fullmoveNumber) =>
         val newSpos = spos :+ SidePieceOption.fromSideAndPiece(side.opposite, Piece.King) :+ SidePieceOption.None
         val shuffledSpos = Random.shuffle(newSpos)
         val (pieces, sq, rSide, res) = f(SidePieceOption.fromSideAndPiece(side, Piece.King) :: spos, side)
         
         ((pieces, side, castling, enPassant, halfmoveClock, fullmoveNumber), sq, rSide, res)
    }
  }
  
  //
  // Testy ataków i szachów.
  //
  
  Seq(("pawn attacks", attackGen(2, pawnAttacks)),
      ("knight attacks", attackGen(8, knightAttacks)),
      ("king attacks", attackGen(8, knightAttacks)),
      ("queen attacks", attackGen(8, queenAttacks)),
      ("bishop attacks", attackGen(4, bishopAttacks)),
      ("rook attacks", attackGen(4, rookAttacks))
      ).foreach {
    case (name, gen) =>
      property("isAttack for " + name + " should return true for attack") = 
        Prop.forAllNoShrink(gen) { case (args, sq, side, res) => newBoardTupled(args).attack(sq, side) == res }
  }

  Seq(("pawn attacks", sideInCheckGen(2, pawnAttacks)),
      ("knight attacks", sideInCheckGen(8, knightAttacks)),
      ("king attacks", sideInCheckGen(8, knightAttacks)),
      ("queen attacks", sideInCheckGen(8, queenAttacks)),
      ("bishop attacks", sideInCheckGen(4, bishopAttacks)),
      ("rook attacks", sideInCheckGen(4, rookAttacks))
      ).foreach {
    case (name, gen) =>
      property("sideInCheck for " + name + " should return true for in check") = 
        Prop.forAllNoShrink(gen) { case (args, sq, side, res) => newBoardTupled(args).sideInCheck(side.opposite) == res }
  }

  Seq(("pawn attacks", inCheckGen(2, pawnAttacks)),
      ("knight attacks", inCheckGen(8, knightAttacks)),
      ("king attacks", inCheckGen(8, knightAttacks)),
      ("queen attacks", inCheckGen(8, queenAttacks)),
      ("bishop attacks", inCheckGen(4, bishopAttacks)),
      ("rook attacks", inCheckGen(4, rookAttacks))
      ).foreach {
    case (name, gen) =>
      property("inCheck for " + name + " should return true for in check") = 
        Prop.forAllNoShrink(gen) { case (args, sq, _, res) => newBoardTupled(args).inCheck == res }
  }
}