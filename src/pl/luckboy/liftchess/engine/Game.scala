package pl.luckboy.liftchess.engine

class Game 
{
  def unsafeSucc[T](move: Move)(z: T)(f: => T): T = throw new Exception
}