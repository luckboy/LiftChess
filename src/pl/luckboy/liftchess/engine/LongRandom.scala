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
import scala.util.Random

/** A class for the generator of pseudo random number. This class used to the generated random number for Zobrist arrays.
 * 
 * @author Łukasz Szpakowski
 */
class LongRandom(val seed: Long) 
{  
  private val mRandom = new Random(seed)
 
  /** Generate the next random number of long type. */
  def nextLong(): Long =
    mRandom.nextLong()
}
