package pl.luckboy.liftchess
import pl.luckboy.liftchess.engine._

/** A class that is extension of the board class.
 * 
 * @author Łukasz Szpakowski
 */
class ImplicitBoard private(bd: Board)
{
  /** The legal moves for board. */
  def legalMoves: Set[Move] = throw new Exception
  
  /** The pseudo legal moves for board. */
  def pseudoLegalMoves: Set[Move] = throw new Exception
  
  /** Returns successor for board if specified move is legal, None otherwise. Successor is copy of board after current side 
   * made move.
   * @param move		the move.
   * @return			the successor.
   */
  def successor(move: Move): Option[Board] = throw new Exception
  
  /** Makes move if specified move is legal. 
   * @param move		the move.
   * @return			true if there made move, false otherwise.
   */
  def makeMove(move: Move): Boolean = throw new Exception
}

/**
 * @author Łukasz Szpakowski
 */
object ImplicitBoard
{
  def boardToImplicitBoard(bd: Board) =
    new ImplicitBoard(bd)
}