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

package pl.luckboy.liftchess.engine

/** A trait for the searches game tree. 
 * 
 * @author Łukasz Szpakowski
 */
trait Searcher
{  
  /** The listener type. */
  type SearchListener
  
  /** The evaluator for the searcher. */
  def evaluator: Evaluator

  /** The game state for the searcher. */
  def gameState: GameState

  /** Sets the game state for the searcher. */
  def gameState_=(gs: GameState): Unit

  /** The search listener. */
  def searchListener: SearchListener

  /** Sets the search listener. */
  def searchListener_=(listener: SearchListener): Unit

  /** Searches game tree for the specified depth.
   * @param depth		the depth.
   * @return			the search result.
   */
  def search(depth: Int): Int
}
