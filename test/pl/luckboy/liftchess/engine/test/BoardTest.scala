package pl.luckboy.liftchess.engine.test
import org.scalacheck._
import org.junit.runner.RunWith

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class BoardTest extends Properties("Board")
{
  property("first test") = Prop.forAll((x: Int, y: Int) => x + y == x + y)
}