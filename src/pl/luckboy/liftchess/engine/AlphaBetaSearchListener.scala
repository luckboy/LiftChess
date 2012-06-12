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

/** A listener trait for alpha beta algorithm.
 * 
 * @author Łukasz Szpakowski
 */
trait AlphaBetaSearchListener
{
  /** Searcher invokes this method if it enters to the node.
   * @param i		the level of the node (zero is root).
   * @param depth	the depth.
   * @param alpha	the alpha.
   * @param beta	the beta.
   */
  def onPreorder(i: Int, depth: Int, alpha: Int, beta: Int): Unit
  
  /** Searcher invokes this method if it leaves from the node.
   * @param i		the level of the node (zero is root).
   * @param depth	the depth.
   * @param alpha	the alpha.
   * @param beta	the beta.
   */
  def onPostorder(i: Int, depth: Int, alpha: Int, beta: Int): Unit
  
  /** Searcher invokes this method if it changes alpha.
   * @param i		the level of the node (zero is root).
   * @param depth	the depth.
   * @param alpha	the alpha.
   * @param move	the move that been reason to change alpha.
   */
  def onAlphaChange(i: Int, depth: Int, alpha: Int, move: Move): Unit
  
  /** Searcher invokes this method if it cut.
   * @param i		the level of the node (zero is root).
   * @param depth	the depth.
   * @param alpha	the alpha.
   * @param move	the move that been reason to cut.
   */
  def onBetaCut(i: Int, depth: Int, alpha: Int, move: Move): Unit
}
