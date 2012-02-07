package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
class MoveStack 
{
  def foldSortedPseudoLegalMoves[T](game: Game)(mvEval: (Move) => Int)(z: T)(p: (T, Move) => T)(f: (T, Move) => T): T = throw new Exception

  def foldSortedGoodPseudoLegalMoves[T](game: Game)(mvEval: (Move) => Int)(z: T)(p: (T, Move) => T)(f: (T, Move) => T): T = throw new Exception
}