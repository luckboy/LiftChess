package pl.luckboy.liftchess.engine

/**
 * @author Łukasz Szpakowski
 */
trait EnumValue 
{
  def id: Int
  
  def name: String
  
  override def toString = name
}
