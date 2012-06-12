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
import pl.luckboy.liftchess.engine.Board

/** A singleton for the FEN.
 * 
 * @author Łukasz Szpakowski
 */
object FEN 
{
  def apply(s: String): Board = 
    FENBuilder(s)
  
  def unapply(bd: Board): Option[String] = 
    FENBuilder.unapply(BoardBuilder.fromBoard(bd))
  
  /** Converts the board to a FEN string.
   * @param bd			the board.
   * @return			a FEN string.
   */
  def toFENString(bd: Board): String =
    FENBuilder.toFENString(BoardBuilder.fromBoard(bd))
}
