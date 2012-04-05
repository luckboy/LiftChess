package pl.luckboy.liftchess.engine
import scala.util.Random

/** A class for generator of pseudo random number. This class used to generated random number for Zobrist arrays.
 * 
 * @author Łukasz Szpakowski
 */
class LongRandom(val seed: Long) 
{  
  private val mRandom = new Random(seed)
 
  /** Generate next random number of long type. */
  def nextLong(): Long =
    mRandom.nextLong()
}