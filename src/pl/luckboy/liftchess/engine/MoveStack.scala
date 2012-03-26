package pl.luckboy.liftchess.engine

/** Klasa stosu ruchów.
 * 
 * @author Łukasz Szpakowski
 */
final class MoveStack(maxDepth: Int, maxMoves: Int)
{
  /** Wskaźnik stosu indeksów. */
  protected var mSp = 0
  
  /** Stos indeksów. */
  protected val mStack = new Array[Int](maxDepth + 1)
  
  /** Początkowy indeks aktualnie wygenerowanych ruchów. */
  protected var mStartMoveIndex = 0
  
  /** Konczowy indeks aktualnie wygenerowanych ruchów. */
  protected var mEndMoveIndex = 0
  
  /** Tablica ruchów (zawiera też ich punkty). */
  protected val mMoves = new Array[Int](maxMoves * 2)
  
  mStack(0) = 0
    
  /** Wkłada ruch na stos.
   * @param piece		bierka.
   * @param src			pole źródła ruchu.
   * @param dst			pole przeznaczenia ruchu.
   * @param promPiece	bierka na którą będzie promowany pionek.
   * @param moveType	typ ruchu.
   */
  private def pushMove(piece: Piece, src: Int, dst: Int, promPiece: PieceOption, moveType: MoveType) = {
    mMoves(mEndMoveIndex) = piece.id | (promPiece.id << 4) | (src << 8) | (dst << 16) | (moveType.id << 24) 
    mEndMoveIndex += 2
  }

  /** Generuje pseudo legalne bicia w przelocie i wkłada na stos.
   * @param bd			plansza.
   */
  private def generatePseudoLegalEnPassants(bd: Board) = {
    val side = bd.side
    val pawn = SidePieceOption.fromSideAndPiece(side, Piece.Pawn)
    bd.enPassant.foldLeft(()) {
      (_, dst) =>
        Square.foldPawnCaptureSquares(dst, side.opposite)(()) { (_, _) => true } {
          (_, src) => if(bd(src) == pawn) pushMove(Piece.Pawn, src, dst, PieceOption.None, MoveType.EnPassant)
        }
    }
  }
  
  /** Generuje pseudo legalne promocje i wkłada na stos.
   * @param bd			plansza.
   * @param src			pole źródła ruchu.
   */
  private def generatePseudoLegalPromotionsFrom(bd: Board, src: Int) =
    Square.foldMoveSquares(src, bd.side, Piece.Pawn)(()) { (_, dst) => bd(dst).isNone } {
      (_, dst) => 
        pushMove(Piece.Pawn, src, dst, PieceOption.Queen, MoveType.NormalMove)
        pushMove(Piece.Pawn, src, dst, PieceOption.Knight, MoveType.NormalMove)
    } {
      (_, dst) =>
        if(!bd(dst).isSide(bd.side)) {
          pushMove(Piece.Pawn, src, dst, PieceOption.Queen, MoveType.Capture)
          pushMove(Piece.Pawn, src, dst, PieceOption.Knight, MoveType.Capture)
        }
    }
  
  /** Generuje pseudo legalne normalne ruchy i bicia i wkłada na stos.
   * @param bd			plansza.
   * @param src			pole źródła ruchu.
   * @param piece		bierka.
   */
  private def generatePseudoLegalNormalMovesAndCapturesFrom(bd: Board, src: Int, piece: Piece) = {
    Square.foldMoveSquares(src, bd.side, piece)(()) { (_, dst) => bd(dst).isNone } {
      (_, dst) => pushMove(piece, src, dst, PieceOption.None, MoveType.NormalMove)
    } {
      (_, dst) => if(!bd(dst).isSide(bd.side)) pushMove(piece, src, dst, PieceOption.None, MoveType.Capture)
    }
  }
    
  /** Generuje pseudo legalne ruchy i wkłada na stos.
   * @param bd			plansza.
   */
  def generatePseudoLegalMoves(bd: Board): Unit = {
    startPushMoves()
    val side = bd.side
    
    // Roszada krótka.
    val row1 = if(side == Side.White) 7 else 0
    if((bd.castling(side) & Castling.KingsideCastling) != Castling.NoneCastling) 
      if(bd(Square(row1, 5)) == SidePieceOption.None && bd(Square(row1, 6)) == SidePieceOption.None)
        pushMove(Piece.King, 4, 6, PieceOption.None, MoveType.KingsideCastling)
    // Roszada długa.
    if((bd.castling(side) & Castling.QueensideCastling) != Castling.NoneCastling)
      if(bd(Square(row1, 1)) == SidePieceOption.None && bd(Square(row1, 2)) == SidePieceOption.None && bd(Square(row1, 3)) == SidePieceOption.None)
        pushMove(Piece.King, 4, 2, PieceOption.None, MoveType.QueensideCastling)
    // Inne ruchy.
    val promSrcRow = if(side == Side.White) 1 else 6
    bd.foldAllSidePieces(side)(()) { (_, _) => true } {
      (_, src) =>
        bd(src).foldPiece(()) {
          (_, piece) => 
            if(piece == Piece.Pawn && Square.toRow(src) == promSrcRow)
              // Promocje.
              generatePseudoLegalPromotionsFrom(bd, src)
            else
              // Nie promocje.
              generatePseudoLegalNormalMovesAndCapturesFrom(bd, src, piece)
        }
    }
    // Bicia w przelocie.
    generatePseudoLegalEnPassants(bd)
  }

  /** Generuje pseudo legalne bicia i wkłada na stos.
   * @param bd			plansza.
   * @param src			pole źródła ruchu.
   * @param piece		bierka.
   */
  private def generatePseudoLegalCapturesFrom(bd: Board, src: Int, piece: Piece) = {
    Square.foldMoveSquares(src, bd.side, piece)(()) { (_, dst) => bd(dst).isNone } { (_, _) => () } {
      (_, dst) => if(!bd(dst).isSide(bd.side)) pushMove(piece, src, dst, PieceOption.None, MoveType.Capture)
    }
  }
  /** Generuje pseudo legalne ruchy które mogą być potencjalnie dobre i wkłada na stos.
   * @param bd			plansza.
   */
  def generatePseudoLegalGoodMoves(bd: Board): Unit = {
    startPushMoves()
    val side = bd.side

    // Inne ruchy.
    val promSrcRow = if(side == Side.White) 1 else 6
    bd.foldAllSidePieces(side)(()) { (_, _) => true } {
      (_, src) =>
        bd(src).foldPiece(()) {
          (_, piece) => 
            if(piece == Piece.Pawn && Square.toRow(src) == promSrcRow)
              // Promocje.
              generatePseudoLegalPromotionsFrom(bd, src)
            else
              // Bicia
              generatePseudoLegalCapturesFrom(bd, src, piece)
        }
    }
    // Bicia w przelocie.
    generatePseudoLegalEnPassants(bd)
  }

  /** Rozpoczyna wstawianie nowych ruchów. Używane w metodach generujących ruchy. */
  private def startPushMoves() = {
    mSp += 1
    mStack(mSp) = mEndMoveIndex
    mStartMoveIndex = mEndMoveIndex
  }

  /** Zdejmuje ruchy z stosu. */
  def popMoves(): Unit = {
    mEndMoveIndex = mStartMoveIndex
    mSp -= 1
    mStartMoveIndex = mStack(mSp)
  }
  
  /** Wkłada pseudo legalne ruchy i wykonuje funkcje. Następnie po wykonaniu funkcji zdejmuje ruchy z stosu.
   * @param bd			plansza.
   * @param f			funkcja.
   * @return			wynik funkcji.
   */
  def generatePseudoLegalMovesWithPopMoves[T](bd: Board)(f: => T): T = {
    generatePseudoLegalMoves(bd)
    val y = f
    popMoves()
    y
  }
  
  /** Wkłada pseudo legalne ruchy, które są potencjalie dobre i wykonuje funkcje. Następnie po wykonaniu funkcji 
   * zdejmuje ruchy z stosu.
   * @param bd			plansza.
   * @param f			funkcja.
   * @return			wynik funkcji.
   */
  def generatePseudoLegalGoodMovesWithPopMoves[T](bd: Board)(f: => T): T = {
    generatePseudoLegalGoodMoves(bd)
    val y = f
    popMoves()
    y
  }

  /** Liczba ruchów. */
  def size: Int =
    (mEndMoveIndex - mStartMoveIndex) >> 1
  
  /** Podaje ruch o danym indeksie.
   * @param i			indeks.
   * @return			ruch.
   */
  def move(i: Int): Move = {
    val data = mMoves(mStartMoveIndex + (i << 1))
    Move(Piece(data & 15), (data >> 8) & 255, (data >> 16) & 255, PieceOption((data >> 4) & 15), MoveType(data >> 24))
  }
  
  /** Podaje punkty ruchu o danym indeksie.
   * @param i			indeks.
   * @return 			ruch.
   */
  def score(i: Int): Int =
    mMoves(mStartMoveIndex + (i << 1) + 1)
  
  /** Ustawia punkty ruchu o danym indeksie.
   * @param i			indeks.
   * @param score		punkty.
   */
  def setScore(i: Int, score: Int): Unit =
    mMoves(mStartMoveIndex + (i << 1) + 1) = score 
  
  /** Zamienia mieściami ruchy i ich punkty o danych indeksach.
   * @param	i			indeks ruchu pierwszego.
   * @param j			indeks ruchu drugiego.
   */
  def swap(i: Int, j: Int): Unit = {
    val ii = mStartMoveIndex + (i << 1)
    val jj = mStartMoveIndex + (j << 1)
    val tmpMove = mMoves(ii)
    val tmpScore = mMoves(ii + 1)
    mMoves(ii) = mMoves(jj)
    mMoves(ii + 1) = mMoves(jj + 1)
    mMoves(jj) = tmpMove
    mMoves(jj + 1) = tmpScore
  }
}