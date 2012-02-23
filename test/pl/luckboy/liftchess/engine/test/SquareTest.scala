package pl.luckboy.liftchess.engine.test
import org.scalacheck._
import org.junit.runner.RunWith
import scala.util.Random
import pl.luckboy.liftchess.engine._

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class SquareTest extends Properties("Square")
{
  property("apply should return square of row and column") =
	Prop.forAll(Gen.choose(0, 7), Gen.choose(0, 7)) {
	  (row, col) => Square(row, col) == (row * 8) + col
	}

  property("toColumn should return column") =
	Prop.forAll(Gen.choose(0, 7), Gen.choose(0, 7)) {
	  (row, col) => Square.toColumn(Square(row, col)) == col
	}

  property("toRow should return row") =
	Prop.forAll(Gen.choose(0, 7), Gen.choose(0, 7)) {
	  (row, col) => Square.toRow(Square(row, col)) == row
	}

  //
  // Testy pól ruchów.
  //
  
  val Border = Seq(
      true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,
      true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,
      true,  true,  false, false, false, false, false, false, false, false, true,  true,
      true,  true,  false, false, false, false, false, false, false, false, true,  true,
      true,  true,  false, false, false, false, false, false, false, false, true,  true,
      true,  true,  false, false, false, false, false, false, false, false, true,  true,
      true,  true,  false, false, false, false, false, false, false, false, true,  true,
      true,  true,  false, false, false, false, false, false, false, false, true,  true,
      true,  true,  false, false, false, false, false, false, false, false, true,  true,
      true,  true,  false, false, false, false, false, false, false, false, true,  true,
      true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,
      true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true,  true
      )
  
  def squareMarks(marks: Seq[Boolean])(sq: Int) = {
    val col = Square.toColumn(sq)
    val row = Square.toRow(sq)
    val newMarks = (Seq.fill(row * 12 + col)(false) ++ marks).take(12 * 12)
    (0 until (12 * 12)).filterNot { i => Border(i) }.map { i => newMarks(i) }
  }

  def pawnMoveSquareMarks(sq: Int, side: Side) =
    side match {
      case Side.White =>
        squareMarks(Seq(
            false, false, true,  false, false, false, false, false, false, false, false, false,
            false, false, true,  false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false
            ))(sq)
      case Side.Black =>
        squareMarks(Seq(
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, true,  false, false, false, false, false, false, false, false, false,
            false, false, true,  false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false
            ))(sq)
    }
  
  def pawnCaptureSquareMarks(sq: Int, side: Side) =
    side match {
      case Side.White =>
        squareMarks(Seq(
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, true,  false, true,  false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false
            ))(sq)
      case Side.Black =>
        squareMarks(Seq(
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, true,  false, true,  false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false
            ))(sq)
    }
  
  val knightMoveSquareMarks =
    squareMarks(Seq(
        false, true,  false, true,  false, false, false, false, false, false, false, false,
        true,  false, false, false, true,  false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        true,  false, false, false, true,  false, false, false, false, false, false, false,
        false, true,  false, true,  false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false
    	)) _
    	
  val kingMoveSquareMarks = 
    squareMarks(Seq(
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, true,  true,  true,  false, false, false, false, false, false, false, false,
        false, true,  false, true,  false, false, false, false, false, false, false, false,
        false, true,  true,  true,  false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false, false, false
    	)) _
    	
  def slideMoveSquareMarks(steps: Seq[(Int, Int)])(limits: Seq[Int], sq: Int) = {
    val col = Square.toColumn(sq)
    val row = Square.toRow(sq)
    val sqs = steps.zip(limits).foldLeft(List[Int]()) {
      case (sqs, ((rowStep, colStep), limit)) =>
        (1 to limit).foldLeft((sqs, row + rowStep, col + colStep)) { 
          case ((sqs, newRow, newCol),  _) =>
            if(!Border((newRow + 2) * 12 + newCol + 2))
              ((Square(newRow, newCol) :: sqs), newRow + rowStep, newCol + colStep)
            else
              (sqs, newRow, newCol)
        }._1
    }
    (0 to 63).map { sqs.contains(_) }
  }
  
  val bishopMoveSquareMarks =
    slideMoveSquareMarks(Seq((-1, -1), (-1, 1), (1, -1), (1, 1))) _
  
  val rookMoveSquareMarks =
    slideMoveSquareMarks(Seq((-1, 0), (0, -1), (0, 1), (1, 0))) _
    
  val queenMoveSquareMakrs = 
    slideMoveSquareMarks(Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))) _    
}