package pl.luckboy.liftchess
import pl.luckboy.liftchess.engine._

/** A class that is extension of the board class.
 * 
 * @author Łukasz Szpakowski
 */
class ImplicitBoard private(bd: Board)
{
  /** Returns legal moves for board. */
  def legalMoves: Set[Move] = 
    pseudoLegalMoves.filter { bd.clone().unsafeFoldSuccessor(_)(false) { (_, _) => true } }
  
  /** Returns pseudo legal moves for board. */
  def pseudoLegalMoves: Set[Move] = {
    val mvStack = new MoveStack(1, 256)
    mvStack.generatePseudoLegalMovesWithPopMoves(bd) { (0 until mvStack.size).map(mvStack.move) }.toSet
  }
  
  /** Returns successor for board if specified move is legal, None otherwise. Successor is copy of board after current side 
   * made move.
   * @param move		the move.
   * @return			the successor.
   */
  def successor(move: Move): Option[Board] = {
    val newBd = bd.clone()
    if(new ImplicitBoard(newBd).makeMove(move)) Some(newBd) else None
  }
  
  /** Makes move if specified move is legal. 
   * @param move		the move.
   * @return			true if there made move, false otherwise.
   */
  def makeMove(move: Move): Boolean =
    pseudoLegalMoves.find(move == _).map { _ => bd.unsafeMakeMove(move).isDefined }.getOrElse(false)
}

/**
 * @author Łukasz Szpakowski
 */
object ImplicitBoard
{
  def boardToImplicitBoard(bd: Board) =
    new ImplicitBoard(bd)
}