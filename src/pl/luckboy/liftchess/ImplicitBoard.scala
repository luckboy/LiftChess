/*******************************************************************************
 * Copyright (C) 2012 Łukasz Szpakowski.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package pl.luckboy.liftchess
import pl.luckboy.liftchess.engine._

/** A class that is extension of the board class.
 * 
 * @author Łukasz Szpakowski
 */
class ImplicitBoard private(bd: Board)
{
  /** Returns a legal moves for the board. */
  def legalMoves: Set[Move] = 
    pseudoLegalMoves.filter { bd.clone().unsafeFoldSuccessor(_)(false) { (_, _) => true } }
  
  /** Returns a pseudo legal moves for the board. */
  def pseudoLegalMoves: Set[Move] = {
    val mvStack = new MoveStack(1, 256)
    mvStack.generatePseudoLegalMovesWithPopMoves(bd) { (0 until mvStack.size).map(mvStack.move) }.toSet
  }
  
  /** Returns the successor for board if specified move is legal, None otherwise. The successor is copy of board after 
   * current side made the move.
   * @param move		the move.
   * @return			the successor.
   */
  def successor(move: Move): Option[Board] = {
    val newBd = bd.clone()
    if(new ImplicitBoard(newBd).makeMove(move)) Some(newBd) else None
  }
  
  /** Makes the move if the specified move is legal. 
   * @param move		the move.
   * @return			true if there made move, false otherwise.
   */
  def makeMove(move: Move): Boolean =
    pseudoLegalMoves.find(move == _).map { bd.unsafeMakeMove(_).isDefined }.getOrElse(false)
  
  /** Returns true if current side is in checkmate. */
  def inCheckmate: Boolean =
    legalMoves.isEmpty && bd.inCheck
  
  /** Returns true if current side is in stalemate. */
  def inStalemate: Boolean =
    legalMoves.isEmpty && !bd.inCheck
}

/**
 * @author Łukasz Szpakowski
 */
object ImplicitBoard
{
  implicit def boardToImplicitBoard(bd: Board): ImplicitBoard =
    new ImplicitBoard(bd)
}
