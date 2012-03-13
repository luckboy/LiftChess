package pl.luckboy.liftchess.engine.test
import org.scalacheck._
import org.junit.runner.RunWith
import scala.util.Random
import pl.luckboy.liftchess.engine._

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class BoardTest extends Properties("Board")
{
  // TODO Dodać generatory combinacji dla wykonywania ruchów.
  // TODO Dodać generatory nielegalnych roszad.
  // TODO Posprawdzać dobrze testy.
  
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

  property("side should return side") =
    Prop.forAllNoShrink(boardArgsGen) { args => newBoardTupled(args).side == args._2 }
  
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
  
  val castlingPiecesGen = Gen.value(Seq(BR, __, __, __, BK, __, __, BR) ++ Seq.fill(6 * 8)(SidePieceOption.None) ++ Seq(WR, __, __, __, WK, __, __, WR))
  
  val castlingGen = Gen.oneOf(
      Castling.NoneCastling,
      Castling.KingsideCastling,
      Castling.QueensideCastling,
      Castling.AllCastling)

  val castlingBoardArgsGen = castlingPiecesGen.map6(sideGen, castlingGen.map2(castlingGen) { case p => p }, SquareOption.None, halfmoveClockGen, fullmoveNumberGen) { case args => args }
  
  property("castling should return castling") = 
    Prop.forAllNoShrink(castlingBoardArgsGen) { 
      args => 
        val bd = newBoardTupled(args)
        (bd.castling(Side.White), bd.castling(Side.Black)) == args._3
    }
  
  val enPassantBoardArgsGen = Gen.choose(0, 6).map5(sideGen, (Castling.NoneCastling, Castling.NoneCastling), halfmoveClockGen, fullmoveNumberGen) {
    (col, side, castling, halfmoveClock, fullmoveNumber) => 
      val spos = Seq(
          SidePieceOption.fromSideAndPiece(side.opposite, Piece.Pawn),
          SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
          )
      val line = (Seq.fill(col)(SidePieceOption.None) ++ spos ++ Seq.fill(8)(SidePieceOption.None)).take(8)
      val pieces = side match {
        case Side.White =>
          Seq(__, __, __, __, BK, __, __, __) ++ Seq.fill(2 * 8)(__) ++ line ++ Seq.fill(3 * 8)(__) ++ Seq(__, __, __, __, WK, __, __, __)
        case Side.Black =>
          Seq(__, __, __, __, BK, __, __, __) ++ Seq.fill(3 * 8)(__) ++ line ++ Seq.fill(2 * 8)(__) ++ Seq(__, __, __, __, WK, __, __, __)
      }
      val enPassant = side match {
        case Side.White => SquareOption(Square(2, col))
        case Side.Black => SquareOption(Square(5, col))
      }
      (pieces, side, castling, enPassant, halfmoveClock, fullmoveNumber)
  }
  
  property("enPassant should return none") = 
    Prop.forAllNoShrink(boardArgsGen) { 
      args => newBoardTupled(args).enPassant == SquareOption.None
    }

  property("enPassant should return en passant square") = 
    Prop.forAllNoShrink(enPassantBoardArgsGen) { 
      args => newBoardTupled(args).enPassant == args._4
    }

  property("halfmoveClock should return halfmove clock") = 
    Prop.forAllNoShrink(boardArgsGen) {
      args => newBoardTupled(args).halfmoveClock == args._5
    }

  property("fullmoveNumber should return fullmove number") = 
    Prop.forAllNoShrink(boardArgsGen) { 
      args => newBoardTupled(args).fullmoveNumber == args._6
    }
    
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
           val newSpos = spos :+ SidePieceOption.fromSideAndPiece(aSide, Piece.King) :+ SidePieceOption.None
           val shuffledSpos = Random.shuffle(newSpos)
           val (pieces, sq, rSide, res) = f(SidePieceOption.fromSideAndPiece(aSide.opposite, Piece.King) :: shuffledSpos, aSide)
           
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
         val (pieces, sq, rSide, res) = f(SidePieceOption.fromSideAndPiece(side, Piece.King) :: shuffledSpos, side.opposite)
         
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
      property("attack for " + name + " should return true for attack") = 
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
  
  def newCapture(piece: Piece, src: Int, dst: Int, promPiece: PieceOption) = 
    Capture(piece, src, dst, promPiece)
    
  def newMoveOrCapture(piece: Piece, src: Int, dst: Int, promPiece: PieceOption, args: BoardArgs) = {
    args._1(dst) match {
      case SidePieceOption.None => NormalMove(piece, src, dst, promPiece)
      case _                    => Capture(piece,src, dst, promPiece)
    }
  }
  
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
  
  def foldSuccessorPropForLegalMoves(gen: Gen[(BoardArgs, List[(Move, BoardArgs)])])(foldSuccessor: (Board) => (Move) => (Boolean) => ((Boolean, Board) => Boolean) => Boolean) = {
    Prop.forAllNoShrink(gen) {
      case (ba, mbas) => { 
        def g(bd: Board, ba: BoardArgs, mbas: List[(Move, BoardArgs)]): Boolean = {
          val res1 = boardTest(bd, ba)
          val res2 = mbas match {
            case (move, ba2) :: mbas2 => 
              foldSuccessor(bd)(move)(false) { 
                (_, bd2) => g(bd2, ba2, mbas2)
              }
            case Nil                  =>
              true
          }
          val res3 = boardTest(bd, ba)          
          res1 && res2 && res3
        }
        
        g(newBoardTupled(ba), ba, mbas)
      }
    }
  }
  
  def foldSuccessorPropForIllegalMoves(gen: Gen[(BoardArgs, Set[Move])])(foldSuccessor: (Board) => (Move) => (Boolean) => ((Boolean, Board) => Boolean) => Boolean) = {
    Prop.forAllNoShrink(gen) {
      case (ba, moves) => 
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
  // Generatory dla testów legalnych ruchów.
  //
  
  val promotionPieceGen = Gen.oneOf(Piece.Knight, Piece.Bishop, Piece.Rook, Piece.Queen)
  
  def sidePieceOptionGenWithSideAndWithoutKing(side: Side) = Gen.oneOf(
      SidePieceOption.None,
      SidePieceOption.fromSideAndPiece(side, Piece.Pawn),
      SidePieceOption.fromSideAndPiece(side, Piece.Knight),
      SidePieceOption.fromSideAndPiece(side, Piece.Bishop),
      SidePieceOption.fromSideAndPiece(side, Piece.Rook),
      SidePieceOption.fromSideAndPiece(side, Piece.Queen))
  
  val pieceGenWithoutKing = Gen.oneOf(Piece.Pawn, Piece.Knight, Piece.Bishop, Piece.Rook, Piece.Queen)
      
  // Ruchy.

  val movesGen = {
	sideGen.flatMap {
	  side =>
	    val osGen = Gen.listOfN(5, sidePieceOptionGenWithSideAndWithoutKing(side.opposite))
	    val ssGen = Gen.listOfN(2, sidePieceOptionGenWithSideAndWithoutKing(side))
	    Gen.choose(0, 255).map6(pieceGenWithoutKing, osGen, ssGen, halfmoveClockGen, fullmoveNumberGen) {
	      (i, piece, os, ss, halfmoveClock, fullmoveNumber) =>
	        val newOs = Random.shuffle(os) :+ SidePieceOption.fromSideAndPiece(side.opposite, Piece.King)
	        val newSs = Random.shuffle(ss) :+ (if(piece == Piece.King) SidePieceOption.None else SidePieceOption.fromSideAndPiece(side, Piece.King))
	        movesFun(i, side, piece, newOs ++ newSs, halfmoveClock, fullmoveNumber)
	    }
	}
  }
  
  def movesFun(i: Int, side: Side, piece: Piece, spos: Seq[SidePieceOption], halfmoveClock: Int, fullmoveNumber: Int) = {
    val s0 = SidePieceOption.fromSideAndPiece(side, piece)
    val Seq(o1, o2, o3, o4, o5, o6, s1, s2, s3) = spos
    import scala.collection.mutable.Seq
    val pieces = Seq(
        __, __, __, __, __, __, __, s3,
        __, __, o1, __, __, __, __, __,
        s2, o2, __, __, o3, __, __, __,
        __, __, __, s0, __, __, o5, __,
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, s1, __, __,
        o4, __, __, __, __, __, __, __,
        __, __, __, __, o6, __, __, __
        )
    val ba = (pieces.toSeq, side, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock, fullmoveNumber)
    val src = Square(3, 3)

    val n = Square.foldMoveSquares(src, side, piece)(0) { (_, sq) => pieces(sq).isNone } { (n, _) => n + 1 } { (n, _) => n + 1 }
    val (dst, _) = Square.foldMoveSquares(src, side, piece)(-1, 0) { (_, sq) => pieces(sq).isNone } { 
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
    
    val move = newMoveOrCapture(piece, src, dst, PieceOption.None, ba)
    
    val ba2 = (pieces2.toSeq, side.opposite, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock2, fullmoveNumber2)
        
    (ba, List((move, ba2)))
  }
  
  // Ruchy króla.
  
  def kingMovesGen = Gen.choose(0, 4).map4(sideGen, halfmoveClockGen, fullmoveNumberGen) {
    (i, side, halfmoveClock, fullmoveNumber) => kingMovesFun(i, side, halfmoveClock, fullmoveNumber)
  }
  
  def kingMovesFun(i: Int, side: Side, halfmoveClock: Int, fullmoveNumber: Int) = {
    val piece = SidePieceOption.fromSideAndPiece(side, Piece.King)
    val sk = piece    
    val ob = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Bishop)
    val or = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Rook)
    val ok = SidePieceOption.fromSideAndPiece(side.opposite, Piece.King)
    import scala.collection.mutable.Seq
    val pieces = Seq(
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, ob, __, __, __, __, __, __,
        __, __, sk, or, __, __, __, __,
        __, ob, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, ok, __, __
        )
    val ba = (pieces.toSeq, side, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock, fullmoveNumber)
    val src = Square(3, 2)
    val dst = Seq(Square(2, 1), Square(2, 2), Square(3, 3), Square(4, 1), Square(4, 2))(i)

    val pieces2 = pieces.clone()
    pieces2(src) = SidePieceOption.None
    pieces2(dst) = piece 
    
    val halfmoveClock2 = if(pieces(dst) == SidePieceOption.None) (halfmoveClock + 1) else 0
    val fullmoveNumber2 = side match {
       case Side.White => fullmoveNumber
       case Side.Black => fullmoveNumber + 1
    }
    
    val move = newMoveOrCapture(Piece.King, src, dst, PieceOption.None, ba)

    val ba2 = (pieces2.toSeq, side.opposite, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock2, fullmoveNumber2)
        
    (ba, List((move, ba2)))
  }
  
  // Promocje.

  val promotionsGen = {
    sideGen.flatMap {
      side => 
	    val osGen = Gen.listOfN(1, sidePieceOptionGenWithSideAndWithoutKing(side.opposite))
	    val ssGen = Gen.listOfN(2, sidePieceOptionGenWithSideAndWithoutKing(side))
	    Gen.choose(0, 255).map6(promotionPieceGen, osGen, ssGen, halfmoveClockGen, fullmoveNumberGen) {
	      (i, promPiece, os, ss, halfmoveClock, fullmoveNumber) =>
	        promotionsFun(i, side, promPiece, os ++ ss, halfmoveClock, fullmoveNumber)
	    }
    }
  }
  
  def promotionsFun(i: Int, side: Side, promPiece: Piece, spos: Seq[SidePieceOption], halfmoveClock: Int, fullmoveNumber: Int) = {
    val Seq(o1, s1, s2) = spos
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
            	__, __, __, __, __, WK, __, BK
            	),
            Square(1, 2))
      case Side.Black => 
        (
            Seq(__, __, __, __, __, WK, __, BK,
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
        
    val move = newMoveOrCapture(Piece.Pawn, src, dst, PieceOption(promPiece.id), ba)
    val ba2 = (pieces2.toSeq, side.opposite, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, 0, fullmoveNumber2)
        
    (ba, List((move, ba2)))
  }
  
  // En passant.
  
  val enPassantsGen = Gen.oneOf(false, true).map4(sideGen, halfmoveClockGen, fullmoveNumberGen)(enPassantsFun)
  
  def enPassantsFun(isEnp: Boolean, side: Side, halfmoveClock: Int, fullmoveNumber: Int) = {
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
            	__, __, __, __, __, WK, __, __
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
    val move2 = if(isEnp)
      EnPassant(src2, capDst2)
    else
      NormalMove(Piece.Pawn, src2, mvDst2, PieceOption.None)
    
    val pieces2 = pieces.clone()
    pieces2(src) = SidePieceOption.None
    pieces2(dst) = SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
    val ba2 = (pieces2.toSeq, side.opposite, (Castling.NoneCastling, Castling.NoneCastling), SquareOption(capDst2), 0, fullmoveNumber2)
    val pieces3 = pieces2.clone()
    if(isEnp) {
      pieces3(src2) = SidePieceOption.None
      pieces3(move.destination) = SidePieceOption.None
      pieces3(capDst2) = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Pawn)
    } else {
      pieces3(src2) = SidePieceOption.None
      pieces3(mvDst2) = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Pawn)          
    }
    val ba3 = (pieces3.toSeq, side, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, 0, fullmoveNumber3)

    (ba, List((move, ba2), (move2, ba3)))
  }
  
  // Roszada
  
  val castlingsGen = Gen.choose(0, 4).map6(Gen.choose(0, 255), sideGen, castlingGen, halfmoveClockGen, fullmoveNumberGen)(castlingsFun)
  
  def castlingsFun(i: Int, j: Int, side: Side, oppCastling: Castling, halfmoveClock: Int, fullmoveNumber: Int) = {
    val (pieces, (move, pieces2, (castling, castling2))) = side match {
      case Side.White =>
        (
            // pieces
            Seq(BR, __, __, __, BK, __, __, BR,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                WP, __, __, __, __, __, __, WP,
                WR, __, __, __, WK, __, __, WR
            	),
            // (move, pieces2, (castling, castling2))
            Seq(
                // Roszada krókta.
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
                    Seq(((Castling.KingsideCastling, oppCastling),  (Castling.NoneCastling, oppCastling)),
                    	((Castling.AllCastling, oppCastling),       (Castling.NoneCastling, oppCastling)))(j % 2)),
                // Roszada długa.
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
                    Seq(((Castling.KingsideCastling, oppCastling),  (Castling.NoneCastling, oppCastling)),
                    	((Castling.AllCastling, oppCastling),       (Castling.NoneCastling, oppCastling)))(j % 2)),
                // Ruch prawą wieżą.
                (
                    NormalMove(Piece.Rook, Square(7, 7), Square(7, 6), PieceOption.None), 
                    Seq(BR, __, __, __, BK, __, __, BR,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	WP, __, __, __, __, __, __, WP,
                    	WR, __, __, __, WK, __, WR, __
                        ),
                    Seq(((Castling.NoneCastling, oppCastling),      (Castling.NoneCastling, oppCastling)),
                    	((Castling.KingsideCastling, oppCastling),  (Castling.NoneCastling, oppCastling)),
                    	((Castling.QueensideCastling, oppCastling), (Castling.QueensideCastling, oppCastling)),
                    	((Castling.AllCastling, oppCastling),       (Castling.QueensideCastling, oppCastling)))(j % 4)),
                // Ruch lewą wieżą.
                (
                    NormalMove(Piece.Rook, Square(7, 0), Square(7, 1), PieceOption.None),
                    Seq(BR, __, __, __, BK, __, __, BR,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	WP, __, __, __, __, __, __, WP,
                    	__, WR, __, __, WK, __, __, WR
                    	),
                    Seq(((Castling.NoneCastling, oppCastling),      (Castling.NoneCastling, oppCastling)),
                        ((Castling.KingsideCastling, oppCastling),  (Castling.KingsideCastling, oppCastling)),
                        ((Castling.QueensideCastling, oppCastling), (Castling.NoneCastling, oppCastling)),
                        ((Castling.AllCastling, oppCastling),       (Castling.KingsideCastling, oppCastling)))(j % 4)),
                // Ruch królem.
                (
                    NormalMove(Piece.King, Square(7, 4), Square(7, 3), PieceOption.None),
                    Seq(BR, __, __, __, BK, __, __, BR,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	WP, __, __, __, __, __, __, WP,
                    	WR, __, __, WK, __, __, __, WR
                    	),
                    Seq(((Castling.NoneCastling, oppCastling),      (Castling.NoneCastling, oppCastling)),
                    	((Castling.KingsideCastling, oppCastling),  (Castling.NoneCastling, oppCastling)),
                    	((Castling.QueensideCastling, oppCastling), (Castling.NoneCastling, oppCastling)),
                    	((Castling.AllCastling, oppCastling),       (Castling.NoneCastling, oppCastling)))(j % 4))
                )(i)
            )
      case Side.Black => 
        (
            // pieces
            Seq(BR, __, __, __, BK, __, __, BR,
                BP, __, __, __, __, __, __, BP,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                WR, __, __, __, WK, __, __, WR
                ),
            // (move, pieces2, (castling, castling2))
            Seq(
                // Roszada krótka.
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
                    Seq(((oppCastling, Castling.KingsideCastling),  (oppCastling, Castling.NoneCastling)),
                    	((oppCastling, Castling.AllCastling),       (oppCastling, Castling.NoneCastling)))(j % 2)),
                // Roszada długa.
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
                    Seq(((oppCastling, Castling.QueensideCastling), (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.AllCastling),       (oppCastling, Castling.NoneCastling)))(j % 2)),
                // Ruch prawą wieżą.
                (
                    NormalMove(Piece.Rook, Square(0, 7), Square(0, 6), PieceOption.None), 
                    Seq(BR, __, __, __, BK, __, BR, __,
                        BP, __, __, __, __, __, __, BP,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        WR, __, __, __, WK, __, __, WR
                	    ),
                    Seq(((oppCastling, Castling.NoneCastling),      (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.KingsideCastling),  (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.QueensideCastling), (oppCastling, Castling.QueensideCastling)),
                        ((oppCastling, Castling.AllCastling),       (oppCastling, Castling.QueensideCastling)))(j % 4)),
                // Ruch lewą wieżą.
                (
                    NormalMove(Piece.Rook, Square(0, 0), Square(0, 1), PieceOption.None), 
                    Seq(__, BR, __, __, BK, __, __, BR,
                	    BP, __, __, __, __, __, __, BP,
                	    __, __, __, __, __, __, __, __,
                	    __, __, __, __, __, __, __, __,
                	    __, __, __, __, __, __, __, __,
                	    __, __, __, __, __, __, __, __,
                	    __, __, __, __, __, __, __, __,
                	    WR, __, __, __, WK, __, __, WR
                       ),
                    Seq(((oppCastling, Castling.NoneCastling),      (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.KingsideCastling),  (oppCastling, Castling.KingsideCastling)),
                        ((oppCastling, Castling.QueensideCastling), (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.AllCastling),       (oppCastling, Castling.KingsideCastling)))(j % 4)),
                // Ruch królem.
                (
                    NormalMove(Piece.King, Square(0, 4), Square(0, 5), PieceOption.None),
                    Seq(BR, __, __, __, __, BK, __, BR,
                        BP, __, __, __, __, __, __, BP,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        WR, __, __, __, WK, __, __, WR
                	    ),
                    Seq(((oppCastling, Castling.NoneCastling),      (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.KingsideCastling),  (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.QueensideCastling), (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.AllCastling),       (oppCastling, Castling.NoneCastling)))(j % 4))
                )(i)
            )
    }
    val fullmoveNumber2 = side match {
      case Side.White => fullmoveNumber
      case Side.Black => fullmoveNumber + 1
    }
    val ba = (pieces, side, castling, SquareOption.None, halfmoveClock, fullmoveNumber)
    val ba2 = (pieces2.toSeq, side.opposite, castling2, SquareOption.None, halfmoveClock + 1, fullmoveNumber2)

    (ba, List((move, ba2)))
  }
  
  // Bicia wież dla roszad.
  
  val lostCastlingsGen = Gen.choose(0, 1).map6(Gen.choose(0, 255), sideGen, castlingGen, halfmoveClockGen, fullmoveNumberGen)(lostCastlingsFun)

  def lostCastlingsFun(i: Int, j: Int, side: Side, oppCastling: Castling, halfmoveClock: Int, fullmoveNumber: Int) = {
    val (pieces, (move, pieces2, (castling, castling2))) = side match {
      case Side.White =>
        (
            // pieces
            Seq(BR, __, __, __, BK, __, __, BR,
                BP, __, __, __, __, __, __, BP,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                __, __, WB, __, __, WB, __, __,
                __, __, __, __, __, __, __, __,
                WR, __, __, __, WK, __, __, WR
            	),
            // (move, pieces2, (castling, castling2))
            Seq(
                // Bicie prawą wieży.
                (
                    newCapture(Piece.Bishop, Square(5, 2), Square(0, 7), PieceOption.None), 
                    Seq(BR, __, __, __, BK, __, __, WB,
                    	BP, __, __, __, __, __, __, BP,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, WB, __, __,
                    	__, __, __, __, __, __, __, __,
                    	WR, __, __, __, WK, __, __, WR
                    	),
                    Seq(((oppCastling, Castling.NoneCastling),      (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.KingsideCastling),  (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.QueensideCastling), (oppCastling, Castling.QueensideCastling)),
                        ((oppCastling, Castling.AllCastling),       (oppCastling, Castling.QueensideCastling)))(j % 4)),
                // Bicie lewą wieżą.
                (
                    newCapture(Piece.Bishop, Square(5, 5), Square(0, 0), PieceOption.None),
                    Seq(WB, __, __, __, BK, __, __, BR,
                        BP, __, __, __, __, __, __, BP,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, WB, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        WR, __, __, __, WK, __, __, WR
                    	),
                    Seq(((oppCastling, Castling.NoneCastling),      (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.KingsideCastling),  (oppCastling, Castling.KingsideCastling)),
                        ((oppCastling, Castling.QueensideCastling), (oppCastling, Castling.NoneCastling)),
                        ((oppCastling, Castling.AllCastling),       (oppCastling, Castling.KingsideCastling)))(j % 4))
                )(i)
            )
      case Side.Black => 
        (
            // pieces
            Seq(BR, __, __, __, BK, __, __, BR,
                __, __, __, __, __, __, __, __,
                __, __, BB, __, __, BB, __, __,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __,
                WP, __, __, __, __, __, __, WP,
                WR, __, __, __, WK, __, __, WR
            	),
            // (move, pieces2, (castling, castling2))
            Seq(
                // Ruch prawą wieżą.
                (
                	newCapture(Piece.Bishop, Square(2, 2), Square(7, 7), PieceOption.None), 
                    Seq(BR, __, __, __, BK, __, __, BR,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, BB, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	__, __, __, __, __, __, __, __,
                    	WP, __, __, __, __, __, __, WP,
                    	WR, __, __, __, WK, __, __, BB
                        ),
                    Seq(((Castling.NoneCastling, oppCastling),      (Castling.NoneCastling, oppCastling)),
                        ((Castling.KingsideCastling, oppCastling),  (Castling.NoneCastling, oppCastling)),
                    	((Castling.QueensideCastling, oppCastling), (Castling.QueensideCastling, oppCastling)),
                    	((Castling.AllCastling, oppCastling),       (Castling.QueensideCastling, oppCastling)))(j % 4)),
                // Ruch lewą wieżą.
                (
                    newCapture(Piece.Bishop, Square(2, 5), Square(7, 0), PieceOption.None), 
                    Seq(BR, __, __, __, BK, __, __, BR,
                        __, __, __, __, __, __, __, __,
                        __, __, BB, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        __, __, __, __, __, __, __, __,
                        WP, __, __, __, __, __, __, WP,
                        BB, __, __, __, WK, __, __, WR
                        ),
                    Seq(((Castling.NoneCastling, oppCastling),      (Castling.NoneCastling, oppCastling)),
                        ((Castling.KingsideCastling, oppCastling),  (Castling.KingsideCastling, oppCastling)),
                        ((Castling.QueensideCastling, oppCastling), (Castling.NoneCastling, oppCastling)),
                        ((Castling.AllCastling, oppCastling),       (Castling.KingsideCastling, oppCastling)))(j % 4))
                )(i)
            )
    }
    val fullmoveNumber2 = side match {
      case Side.White => fullmoveNumber
      case Side.Black => fullmoveNumber + 1
    }
    val ba = (pieces, side, castling, SquareOption.None, halfmoveClock, fullmoveNumber)
    val ba2 = (pieces2.toSeq, side.opposite, castling2, SquareOption.None, 0, fullmoveNumber2)

    (ba, List((move, ba2)))
  }
    
  //
  // Testy wykonywania legalnych ruchów.
  //
  
  def foldSuccessorForUnsafeMakeMove[T](bd: Board)(move: Move)(z: T)(f: (T, Board) => T): T = {
    bd.unsafeMakeMove(move).map {
      undo =>
        val y = f(z, bd)
        bd.unsafeUndoMove(undo)
        y
    }.getOrElse(z)
  }
  
  Seq(("normal moves and captures", movesGen),
      ("king moves", kingMovesGen),
      ("promotions", promotionsGen),
      ("en passants", enPassantsGen),
      ("castlings", castlingsGen),
      ("lost castlings", lostCastlingsGen)
      ).foreach {
    case (name, gen) => {
      property("unsafeFoldSuccessor for " + name + " should make move and undo move") =
        foldSuccessorPropForLegalMoves(gen) { _.unsafeFoldSuccessor }

      property("unsafeMakeMove and unsafeUndoMove for " + name + " should make move and undo move") =
        foldSuccessorPropForLegalMoves(gen)(foldSuccessorForUnsafeMakeMove)
    }
  }
  
  //
  // Generatory dla testów nie legalnych ruchów.
  //

  val illegalKingMovesGen = sideGen.map3(halfmoveClockGen, fullmoveNumberGen)(illegalKingMovesFun)

  def illegalKingMovesFun(side: Side, halfmoveClock: Int, fullmoveNumber: Int) = {
    val (pieces, src, dsts) = side match {
      case Side.White =>
        (
            Seq(__, __, __, __, __, __, __, __,
            	__, __, __, __, __, __, __, __,
            	BP, __, __, __, BK, __, __, __,
            	__, __, __, __, __, __, __, __,
            	__, __, WK, __, __, __, __, __,
            	__, __, __, __, __, __, __, __,
            	__, __, __, __, BN, __, __, __,
            	__, __, __, __, __, __, __, __
            	),
            Square(4, 2),
            Set(Square(3, 1), Square(3, 3), Square(4, 3), Square(5, 2)))
      case Side.Black =>
        (
            Seq(__, __, __, __, __, __, __, WK,
            	__, __, __, WN, __, __, __, __,
            	__, __, __, __, __, __, __, __,
            	__, __, __, __, __, __, __, __,
            	__, __, WB, __, BK, __, __, __,
            	__, __, WP, __, __, __, __, __,
            	__, __, __, __, __, __, __, __,
            	__, __, __, __, __, WR, __, __
            	),
            Square(4, 4),
            Set(Square(3, 3), Square(3, 4), Square(3, 5), Square(4, 3), Square(4, 5), Square(5, 3), Square(5, 5)))
    }
    val ba = (pieces, side, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock, fullmoveNumber)
    val moves = dsts.map { NormalMove(Piece.King, src, _, PieceOption.None) }
    (ba, moves)
  }

  val illegalKingExposuresGen = sideGen.map3(halfmoveClockGen, fullmoveNumberGen)(illegalKingExposuresFun)

  def illegalKingExposuresFun(side: Side, halfmoveClock: Int, fullmoveNumber: Int) = {
    val or = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Rook)
    val oq = SidePieceOption.fromSideAndPiece(side.opposite, Piece.Queen)
    val ok = SidePieceOption.fromSideAndPiece(side.opposite, Piece.King)
    val sn = SidePieceOption.fromSideAndPiece(side, Piece.Knight)
    val sb = SidePieceOption.fromSideAndPiece(side, Piece.Bishop)
    val sk = SidePieceOption.fromSideAndPiece(side, Piece.King)
    val pieces = Seq(
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, sk, sb, __, __, or, ok,
        __, __, __, sn, __, __, __, __,
        __, __, __, __, __, __, __, __,
        __, __, __, __, __, oq, __, __,
        __, __, __, __, __, __, __, __        
        )
    
    val ba = (pieces, side, (Castling.NoneCastling, Castling.NoneCastling), SquareOption.None, halfmoveClock, fullmoveNumber)
    val moves = Set((Piece.Bishop, Square(3, 3)), (Piece.Knight, Square(4, 3))).flatMap {
      case (piece, src) => 
        Square.foldMoveSquares(src, side, piece)(Set[Move]()) { (_, dst) => pieces(dst) == SidePieceOption.None } {
          (moves, dst) => moves + NormalMove(piece, src, dst, PieceOption.None)
        } {
          (moves, _) => moves
        }
    }
    (ba, moves)
  }
  
  Seq(("illegal king moves", illegalKingMovesGen),
      ("illegal king exposures", illegalKingExposuresGen)
      ).foreach {
    case (name, gen) => {
      property("unsafeFoldSuccessor for " + name + " should make move and undo move") =
        foldSuccessorPropForIllegalMoves(gen) { _.unsafeFoldSuccessor }

      property("unsafeMakeMove and unsafeUndoMove for " + name + " should make move and undo move") =
        foldSuccessorPropForIllegalMoves(gen)(foldSuccessorForUnsafeMakeMove)
    }
  }
  
  def stringToPieces(s: String) = s.split(", ").map {
      case "_" => SidePieceOption.None
      case "P" => SidePieceOption.WhitePawn
      case "N" => SidePieceOption.WhiteKnight
      case "B" => SidePieceOption.WhiteBishop
      case "R" => SidePieceOption.WhiteRook
      case "Q" => SidePieceOption.WhiteQueen
      case "K" => SidePieceOption.WhiteKing
      case "p" => SidePieceOption.BlackPawn
      case "n" => SidePieceOption.BlackKnight
      case "b" => SidePieceOption.BlackBishop
      case "r" => SidePieceOption.BlackRook
      case "q" => SidePieceOption.BlackQueen
      case "k" => SidePieceOption.BlackKing
      case _   => throw new Exception
    }.toSeq
}