package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
class Board 
{
  def foldSidePiece[T](side: Side.Value, piece: Piece.Value)(z: T)(f: (T, Int) => T): T = throw new Exception
  
  def foldPieces[T](piece: Piece.Value)(z: T)(f: (T, Int) => T): T = throw new Exception
  
  def foldAllSidePieces[T](side: Side.Value)(z: T)(f: (T, Int) => T): T = throw new Exception

  def foldAllPieces[T](z: T)(f: (T, Int) => T): T = throw new Exception

  def foldMoveSquares[T](piece: Piece.Value)(z: T)(p: (T, Int) => Boolean)(y: (T, Int) => T)(n: (T, Int) => T): T = throw new Exception

  def side: Side.Value = throw new Exception

  def sideCastling(side: Side.Value): Castling.Value = throw new Exception
  
  def enPassant: Option[Int] = throw new Exception
  
  def halfmoveClock: Int = throw new Exception
  
  def fullmoveNumber: Int = throw new Exception
  
  def apply(sq: Int): SidePiece.Value = throw new Exception
  
  def update(sq: Int, pc: SidePiece.Value): Unit = throw new Exception
  
  def unsafeSucc[T](move: Move)(z: T)(f: => T): T = throw new Exception
  
  def attack(sq: Int, side: Side.Value): Boolean = throw new Exception
  
  def sideInCheck(side: Side.Value): Boolean = throw new Exception
  
  def inCheck: Boolean = throw new Exception  
  
  def foldSidePseudoLegalMoves[T](side: Side.Value)(z: T)(f: (T, Move) => T): T = throw new Exception

  def foldSideGoodPseudoLegalMoves[T](side: Side.Value)(z: T)(f: (T, Move) => T): T = throw new Exception

  def foldPseudoLegalMoves[T](z: T)(f: (T, Move) => T): T = throw new Exception

  def foldGoodPseudoLegalMoves[T](z: T)(f: (T, Move) => T): T = throw new Exception
}