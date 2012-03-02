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
  
  def newBoardTupled(args: BoardArgs) =
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

  // Bierki
  
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
  
  //
  // Generatorów ataków i szachów.
  //
  
  def pawnAttacks(spos: Seq[SidePieceOption], side: Side) = {
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
    val res = (
        Seq(a1, a2).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.Pawn) } ||
        Seq(a1, a2).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.Bishop) } ||
        Seq(a1, a2).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.Queen) } ||
        Seq(a1, a2).exists { _ == SidePieceOption.fromSideAndPiece(side, Piece.King) }
        )
     
    (pieces, Square(4, 3), side, res)
  }

  def knightAttacks(spos: Seq[SidePieceOption], side: Side) = {
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
  
  //
  // Fukcje dla testów wykonywania ruchów.
  //
  
  def boardTest(bd: Board, args: BoardArgs) = {
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
  
  def foldSuccessorPropForLegalMoves[T](gen: Gen[T])(f: (T) => (BoardArgs, List[(Move, BoardArgs)]))(foldSuccessor: (Board) => (Move) => (Boolean) => ((Boolean, Board) => Boolean) => Boolean) = {
    Prop.forAllNoShrink(gen) {
      x => { 
        val (ba, mbas) = f(x)
        def g(bd: Board, ba: BoardArgs, mbas: List[(Move, BoardArgs)]): Boolean = {
          val res1 = boardTest(bd, ba)
          val res2 = mbas match {
            case (move, ba2) :: mbas2 => 
              foldSuccessor(bd)(move)(false) { 
                (b, bd2) => b == true && g(bd2, ba2, mbas2)
              }
            case Nil                 =>
              true
          }
          val res3 = boardTest(bd, ba)          
          res1 && res2 && res3
        }
        
        g(newBoardTupled(ba), ba, mbas)
      }
    }
  }
  
  def foldSuccessorPropForIllegalMoves[T](gen: Gen[T])(f: (T) => (BoardArgs, Set[Move]))(foldSuccessor: (Board) => (Move) => (Boolean) => ((Boolean, Board) => Boolean) => Boolean) = {
    Prop.forAllNoShrink(gen) {
      x => 
        val (ba, moves) = f(x)
        moves.forall {
          move => 
            val aBd = newBoardTupled(ba)
            val res = boardTest(aBd, ba)
            val res2 = foldSuccessor(aBd)(move)(true) { (_, _) => false }
            res && boardTest(aBd, ba)
        }
    }
  }
  
  //
  // Generatory dla testów ruchów.
  //
  
  val promotionPieceGen = Gen.oneOf(Piece.Knight, Piece.Bishop, Piece.Rook, Piece.Queen)
  
  // Ruchy.
  
  def movesFun(x: (Int, Side, Piece, Seq[SidePieceOption], Int, Int)) = 
    x match {
      case (i, side, piece, Seq(p1, p2, o1, o2, o3, o4, o5, o6, s1, s2), halfmoveClock, fullmoveNumber) => {
        import scala.collection.mutable.Seq
        val s0 = SidePieceOption.fromSideAndPiece(side, piece)
        val pieces = Seq(
            __, __, __, __, p1, __, __, __,
            __, __, __, __, __, __, __, __,
            s2, o1, __, __, o2, __, __, __,
            __, __, __, s0, __, __, o6, __,
            __, __, __, __, o4, __, __, __,
            __, __, o3, __, __, s1, __, __,
            o5, __, __, __, __, __, __, __,
            __, __, __, __, p2, __, __, __
            )
        val ba = (pieces.toSeq, side, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock, fullmoveNumber)
        val src = Square(3, 3)
        val n = Square.foldMoveSquares(src, side, piece)(0) { (_, sq) => pieces(sq).isNone } { (n, _) => n + 1 } { (n, _) => n + 1 }
        val (dst, _) = Square.foldMoveSquares(src, side, piece)(0, 0) { (_, sq) => pieces(sq).isNone } { 
          case ((oldDst, j), dst) => ((if(j == i % n) dst else oldDst), j + 1)
        } {
          case ((oldDst, j), dst) => ((if(j == i % n) dst else oldDst), j + 1)
        }
        val pieces2 = pieces.clone()
        pieces2(src) = SidePieceOption.None
        pieces2(dst) = s0
        val halfmoveClock2 = piece match {
          case Piece.Pawn => 0
          case _          => if(pieces(dst) == SidePieceOption.None) (halfmoveClock + 1) else 0
        }
        val fullmoveNumber2 = side match {
          case Side.White => fullmoveNumber
          case Side.Black => fullmoveNumber + 1
        }
        val move = NormalMove(piece, src, dst, PieceOption.None)
        val ba2 = (pieces2.toSeq, side.opposite, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock2, fullmoveNumber2)
        
        (ba, List((move, ba2)))
      }
    }
  
  // Promocje.

  def promotionsFun(x: (Int, Side, Piece, Seq[SidePieceOption], Int, Int)) = 
    x match {
      case (i, side, promPiece, Seq(p1, p2, o1, s1, s2), halfmoveClock, fullmoveNumber) => {
        import scala.collection.mutable.Seq
        val (pieces, src) = side match {
          case Side.White =>
            (
                Seq(__, __, __, o1, __, __, __, __,
                    __, __, WP, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, s1, __, s2, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, p2, __, p1
                    ),
                Square(1, 2))
          case Side.Black => 
            (
                Seq(__, __, __, __, __, p2, __, p1,
                    __, s1, __, __, __, __, __, __,
                    s2, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, BP, __, __, __, __,
                    __, __, __, __, o1, __, __, __
                    ),
                Square(6, 3))
        }
        val ba = (pieces.toSeq, side, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock, fullmoveNumber)
        val n = Square.foldMoveSquares(src, side, Piece.Pawn)(0) { (_, sq) => pieces(sq).isNone } { (n, _) => n + 1 } { (n, _) => n + 1 }
        val (dst, _) = Square.foldMoveSquares(src, side, Piece.Pawn)(0, 0) { (_, sq) => pieces(sq).isNone } { 
          case ((oldDst, j), dst) => ((if(j == i % n) dst else oldDst), j + 1)
        } {
          case ((oldDst, j), dst) => ((if(j == i % n) dst else oldDst), j + 1)
        }
        val pieces2 = pieces.clone()
        pieces2(src) = SidePieceOption.None
        pieces2(dst) = SidePieceOption.fromSideAndPiece(side, promPiece)
        val fullmoveNumber2 = side match {
          case Side.White => fullmoveNumber
          case Side.Black => fullmoveNumber + 1
        }
        val move = NormalMove(Piece.Pawn, src, dst, PieceOption(promPiece.id))
        val ba2 = (pieces2.toSeq, side.opposite, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, 0, fullmoveNumber2)
        
        (ba, List((move, ba2)))
      }
    }
  
  // En passant.
  
  def enPassantsFun(x: (Boolean, Side, Int, Int)) = 
    x match {
      case (isEnp, side, halfmoveClock, fullmoveNumber) => {
        import scala.collection.mutable.Seq
        val (pieces, src, dst, src2, mvDst2, capDst2) = side match {
          case Side.White =>
            (
                Seq(__, __, __, __, __, BK, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, BP, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, WP, __, __, __, __, __,
                    __, __, __, __, __, WP, __, __
                    ),
                Square(6, 2), Square(4, 2),
                Square(4, 1), Square(5, 1), Square(5, 2))
          case Side.Black => 
            (
                Seq(__, __, __, __, __, BK, __, __,
                    __, __, BP, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, WP, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, WK, __, __
                    ),
                Square(1, 2), Square(3, 2),
                Square(3, 3), Square(2, 3), Square(2, 2))
        }
        val ba = (pieces.toSeq, side, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock, fullmoveNumber)
        val (fullmoveNumber2, fullmoveNumber3) = side match {
          case Side.White => (fullmoveNumber, fullmoveNumber + 1)
          case Side.Black => (fullmoveNumber + 1, fullmoveNumber + 1)
        }
        val move = NormalMove(Piece.Pawn, src, dst, PieceOption.None)
        val move2 = NormalMove(Piece.Pawn, src2, if(isEnp) capDst2 else mvDst2, PieceOption.None)
        val pieces2 = pieces.clone()
        pieces2(src) = SidePieceOption.None
        pieces2(dst) = SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
        val ba2 = (pieces2.toSeq, side.opposite, (Castling.NoneCastling, Castling.NoneCastling), SquareOption(capDst2), 0, fullmoveNumber2)
        val pieces3 = pieces.clone()
        if(isEnp) {
          pieces3(src2) = SidePieceOption.None
          pieces3(capDst2) = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Pawn)
        } else {
          pieces3(src2) = SidePieceOption.None
          pieces3(mvDst2) = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Pawn)          
        }
        val ba3 = (pieces3.toSeq, side.opposite, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, 0, fullmoveNumber3)

        (ba, List((move, ba2), (move2, ba3)))
      }
    }
  
  // Roszada
  
  def castlingsFun(x: (Boolean, Boolean, Side, Castling, Int, Int)) =
    x match {
      case (isKingside, isAllCastling, side, oppCastling, halfmoveClock, fullmoveNumber) => {
        val (pieces, allCastling, noneCastling, (move, pieces2, castling)) = side match {
          case Side.White =>
            (
                Seq(BR, __, __, __, BK, __, __, BR,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    WP, __, __, __, __, __, __, WP,
                    WR, __, __, __, WK, __, __, WR
                    ),
                (Castling.AllCastling, oppCastling),
                (Castling.NoneCastling, oppCastling),
                if(isKingside) 
                  (
                      KingsideCastling(), 
                      Seq(BR, __, __, __, BK, __, __, BR,
                          __, __, __, __, __, __, __, __,
                          __, __, __, __, __, __, __, __,
                          __, __, __, __, __, __, __, __,
                          __, __, __, __, __, __, __, __,
                          __, __, __, __, __, __, __, __,
                          WP, __, __, __, __, __, __, WP,
                          WR, __, __, __, __, WR, WK, __
                    	  ),
                      (Castling.KingsideCastling, oppCastling)
                      )
                else
                  (
                      QueensideCastling(), 
                      Seq(BR, __, __, __, BK, __, __, BR,
                          __, __, __, __, __, __, __, __,
                          __, __, __, __, __, __, __, __,
                          __, __, __, __, __, __, __, __,
                          __, __, __, __, __, __, __, __,
                          __, __, __, __, __, __, __, __,
                          WP, __, __, __, __, __, __, WP,
                          __, __, WK, WR, __, __, __, WR
                          ),
                      (Castling.QueensideCastling, oppCastling)
                      )
                )
          case Side.Black => 
            (
                Seq(BR, __, __, __, BK, __, __, BR,
                    BP, __, __, __, __, __, __, BP,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    __, __, __, __, __, __, __, __,
                    WR, __, __, __, WK, __, __, WR
                    ),
                (oppCastling, Castling.AllCastling),
                (oppCastling, Castling.NoneCastling),
                if(isKingside) 
                (
                    KingsideCastling(), 
                    Seq(BR, __, __, __, __, BR, BK, __,
                        BP, __, __, __, __, __, __, BP,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        WR, __, __, __, WK, __, __, WR
                        ),
                    (oppCastling, Castling.KingsideCastling)
                    )
                else
                (
                    QueensideCastling(), 
                    Seq(__, __, BK, BR, __, __, __, BR,
                        BP, __, __, __, __, __, __, BP,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        WR, __, __, __, WK, __, __, WR
                        ),
                    (oppCastling, Castling.QueensideCastling)
                    )
                )
        }
        val tmpCastling = if(isAllCastling) allCastling else castling
        val fullmoveNumber2 = side match {
          case Side.White => fullmoveNumber
          case Side.Black => fullmoveNumber + 1
        }
        val ba = (pieces, side, tmpCastling, SquareOption.None, halfmoveClock, fullmoveNumber)
        val ba2 = (pieces2.toSeq, side.opposite, noneCastling, SquareOption.None, 0, fullmoveNumber2)

        (ba, List((move, ba2)))
      }
    }  
}