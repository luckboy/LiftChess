package pl.luckboy.liftchess.engine

/** Klasa planszy. 
 * 
 * @author Łukasz Szpakowski
 */
class Board 
{
  /** Podaje liczbę wystąpień danej bierki danej strony.
   * @param side		strona.
   * @param piece		bierka.
   * @return			liczba bierek.
   */
  def countSidePieces(side: Side, piece: Piece): Int = throw new Exception
  
  /** Podaje liczbę wystąpień bierki.
   * @param piece		bierka.
   * @return			liczba bierek.
   */
  def countPieces(piece: Piece): Int = throw new Exception
  
  /** Podaje liczbę wszystkich bierek danej strony.
   * @param side		strona.
   * @return			liczba bierek.
   */
  def countAllSidePieces(side: Side): Int = throw new Exception
  
  /** Podaje liczbę wszystkich bierek.
   * @return			liczba bierek.
   */
  def countAllPieces: Int = throw new Exception
  
  /** Składa określone bierki danej strony.
   * @param side		strona.
   * @param piece		bierka.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania (gdy false przerywa).
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldSidePieces[T](side: Side, piece: Piece)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = throw new Exception
  
  /** Składa określone bierki.
   * @param piece		bierka.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania (gdy false przerywa).
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldPieces[T](piece: Piece)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = throw new Exception
  
  /** Składa wszystkie bierki danej strony.
   * @param side		strona.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania (gdy false przerywa).
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldAllSidePieces[T](side: Side)(z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = throw new Exception

  /** Składa wszystkie bierki.
   * @param z			wartość początkowa.
   * @param p			funkcja przerwania (gdy false przerywa).
   * @param f			funkcja składania.
   * @return			wynik składania.
   */
  def foldAllPieces[T](z: T)(p: (T, Int) => Boolean)(f: (T, Int) => T): T = throw new Exception

  /** Strona. */
  def side: Side = throw new Exception

  /** Dostępne roszady dla danej strony.
   * @param side		strona.
   * @return			dostępne roszady.
   */
  def castling(side: Side): Castling = throw new Exception
  
  /** Pole bicia w przelocie. */
  def enPassant: SquareOption = throw new Exception

  /** Liczba półruchów. */
  def halfmoveClock: Int = throw new Exception

  /** Liczna ruchów. */
  def fullmoveNumber: Int = throw new Exception
  
  /** Hashowy klucz. */
  def hashKey: Long = throw new Exception
  
  /** Podaje bierkę na określonym polu. */
  def apply(sq: Int): SidePieceOption = throw new Exception
  
  /** Słada następnik planszy. W rzeczywistości wykonuje ruch jeśli jest legalny i wykonuje daną funkcje. Po 
   * wykonaniu funkcji cofa ruch.
   * @param	move		ruch.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik funkcji lub wartość początkowa.
   */
  def unsafeFoldSuccessor[T](move: Move)(z: T)(f: (T, Board) => T): T = throw new Exception

  /** Słada następnik planszy dla ruchu pustego. W rzeczywistości  wykonuje pusty ruch jeśli jest legalny i 
   * wykonuje daną funkcje. Po wykonaniu funkcji cofa ruch pusty.
   * @param z			wartość początkowa.
   * @param f			funkcja.
   * @return			wynik funkcji lub wartość początkowa.
   */  
  def unsafeFoldNullSuccessor[T](z: T)(f: (T, Board) => T): T = throw new Exception
  
  /** Słada następnik planszy bez obliczania hash klucza. W rzeczywistości wykonuje ruch jeśli jest legalny i 
   * wykonuje daną funkcje. Po wykonaniu funkcji cofa ruch.
   * @param	move		ruch.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik funkcji lub wartość początkowa.
   */
  def unsafeFoldSuccessorWithoutHashKey[T](move: Move)(z: T)(f: (T, Board) => T): T = throw new Exception

  /** Słada następnik planszy dla ruchu pustego bez obliczania hash klucza. W rzeczywistości wykonuje pusty ruch 
   * jeśli jest legalnyi wykonuje daną funkcje. Po wykonaniu funkcji cofa ruch pusty.
   * @param z			wartość początkowa.
   * @param f			funkcja składania.
   * @return			wynik funkcji lub wartość początkowa.
   */  
  def unsafeFoldNullSuccessorWithoutHashKey[T](z: T)(f: (T, Board) => T): T = throw new Exception
  
  /** Wykonuje ruch.
   * @param move		ruch.
   * @return			dane używane do cofania ruchu.
   */
  def unsafeMakeMove(move: Move): Option[Undo] = throw new Exception

  /** Cofa ruch.
   * @param undo		dane cofania ruchu.
   */
  def unsafeUndoMove(undo: Undo): Unit = throw new Exception

  
  /** Sprawdza czy dane pole jest atakowane daną stronę.
   * @param sq			pole atakowane.
   * @param side		strona atakującia.
   * @return			jeśli strona atakuje to true.
   */
  def attack(sq: Int, side: Side): Boolean = throw new Exception
  
  /** Sprawdza czy jest szach dla danej strony.
   * @param side		strona.
   * @return			jeśli jest szach to true.
   */
  def sideInCheck(side: Side): Boolean = throw new Exception
  
  /** Sprawdza czy jest szach dla strony która ma ruch. */
  def inCheck: Boolean = throw new Exception
}

/**
 * @author Łukasz Szpakowski
 */
object Board
{
  def apply(
      pieces: Seq[SidePieceOption],
      side: Side,
      canstling: (Castling, Castling), 
      enPassant: SquareOption, 
      halfmoveClock: Int,
      fullmoveNumber: Int): Board = throw new Exception
}