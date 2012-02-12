package pl.luckboy.liftchess.engine

case class SquareOption(sq: Int)
{
  def foldLeft[T](z: T)(f: (T, Int) => T): T = throw new Exception
}