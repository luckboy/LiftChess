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

/** A singleton for the game result.
 * 
 * @author Łukasz Szpakowski
 */
object Result extends Enumeration
{
  val Unknown = Value("*")
  val WhiteWin = Value("1-0")
  val BlackWin = Value("0-1")
  val Draw = Value("1/2-1/2")
  
  def win(side: Side) =
    if(side == Side.White) WhiteWin else BlackWin
    
  def lose(side: Side) =
    if(side == Side.White) BlackWin else WhiteWin
}
