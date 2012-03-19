package pl.luckboy.liftchess.engine.test
import org.scalacheck._
import org.junit.runner.RunWith
import scala.util.Random
import pl.luckboy.liftchess.engine._

@RunWith(classOf[org.scalacheck.contrib.ScalaCheckJUnitPropertiesRunner])
class SquareTest extends Properties("Square")
{
  import TestHelper._
  
  property("apply should return square of row and column") =
	Prop.forAllNoShrink(Gen.choose(0, 7), Gen.choose(0, 7)) {
	  (row, col) => Square(row, col) == (row * 8) + col
	}

  property("toColumn should return column") =
	Prop.forAllNoShrink(Gen.choose(0, 7), Gen.choose(0, 7)) {
	  (row, col) => Square.toColumn(Square(row, col)) == col
	}

  property("toRow should return row") =
	Prop.forAllNoShrink(Gen.choose(0, 7), Gen.choose(0, 7)) {
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
            false, false, false, false, false, false, false, false, false, false, false, false,
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
    	
  def slidingMoveSquareMarks(steps: Seq[(Int, Int)])(limits: Seq[Int], sq: Int) = {
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
    slidingMoveSquareMarks(Seq((-1, -1), (-1, 1), (1, -1), (1, 1))) _
  
  val rookMoveSquareMarks =
    slidingMoveSquareMarks(Seq((-1, 0), (0, -1), (0, 1), (1, 0))) _
    
  val queenMoveSquareMarks = 
    slidingMoveSquareMarks(Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))) _
    
  property("foldPawnCaptureSquares should return square set") =
    Prop.forAllNoShrink(squareGen, sideGen) {
      (sq, side) =>
        val marks = pawnCaptureSquareMarks(sq, side)
        val aSqs = Square.foldPawnCaptureSquares(sq, side)(Set[Int]()) { (_, _) => true }{ (sqs, sq) => sqs + sq }
        val eSqs = (0 to 63).filter { marks }.toSet
        aSqs == eSqs
    }
  
  property("foldPawnMoveSquares should return square set advance one square") =
    Prop.forAllNoShrink(squareGen, sideGen, Gen.listOfN(64, Gen.value(false) | Gen.value(true))) {
      (sq, side, randomMarks) =>
        val marks = pawnMoveSquareMarks(sq, side)
        val aSqs = Square.foldPawnMoveSquares(sq, side)(Set[Int](), true) { case ((_, b), sq) => randomMarks(sq) & b } { 
          case ((sqs, b), sq) => (sqs + sq, false) 
        }._1
        val eSqs = (0 to 63).filter { sq => marks(sq) & randomMarks(sq) }.toSet
        aSqs == eSqs
    }

  property("foldPawnMoveSquares should return square set advance two squares") =
    Prop.forAllNoShrink(squareGen, sideGen, Gen.listOfN(64, Gen.value(false) | Gen.value(true))) {
      (sq1, side, randomMarks) =>
        val col = Square.toColumn(sq1)
        val row = if(side == Side.White) 6 else 1
        val marks1 = pawnMoveSquareMarks(sq1, side)
        val marks2 = if(Square.toRow(sq1) == row) {
          val sq2 = side match {
            case Side.White => Square(5, col)
            case Side.Black => Square(2, col)
          }
          pawnMoveSquareMarks(sq2, side)
        } else {
          Seq.fill(64)(false)
        }
        val aSqs = Square.foldPawnMoveSquares(sq1, side)(Set[Int](), true) { case ((_, b), sq) => randomMarks(sq) | b } { 
          case ((sqs, b), sq) => (sqs + sq, false)
        }._1
        val eSqs = (0 to 63).filter { sq => marks1(sq) | (marks2(sq) & randomMarks(sq)) }.toSet
        aSqs == eSqs
    }
  
  property("foldMoveSquares for P should return two  square sets") =
    Prop.forAllNoShrink(squareGen, sideGen, Gen.listOfN(64, Gen.value(false) | Gen.value(true))) {
	  (sq, side, randomMarks) =>
	    val mvMarks = pawnMoveSquareMarks(sq, side)
	    val capMarks = pawnCaptureSquareMarks(sq, side)
	    val (aSqs1, aSqs2, _) = Square.foldMoveSquares(sq, side, Piece.Pawn)(Set[Int](), Set[Int](), true) { case ((_, _, b), sq) => randomMarks(sq) & (b | capMarks(sq)) } {
	      case ((sqs1, sqs2, _), sq) => (sqs1 + sq, sqs2, false)
	    } {
	      case ((sqs1, sqs2, _), sq) => (sqs1, sqs2 + sq, true)
	    }
	    val eSqs1 = (0 to 63).filter { sq => mvMarks(sq) & randomMarks(sq) }.toSet
	    val eSqs2 = (0 to 63).filter { sq => capMarks(sq) & !randomMarks(sq) }.toSet
	    aSqs1 == eSqs1 && aSqs2 == eSqs2 
    }

  property("foldMoveSquares for P should return square set advance two squares") =
    Prop.forAllNoShrink(squareGen, sideGen, Gen.listOfN(64, Gen.value(false) | Gen.value(true))) {
      (sq1, side, randomMarks) =>
        val col = Square.toColumn(sq1)
        val row = if(side == Side.White) 6 else 1
        val mvMarks1 = pawnMoveSquareMarks(sq1, side)
        val mvMarks2 = if(Square.toRow(sq1) == row) {
          val sq2 = side match {
            case Side.White => Square(5, col)
            case Side.Black => Square(2, col)
          }
          pawnMoveSquareMarks(sq2, side)
        } else {
          Seq.fill(64)(false)
        }
	    val capMarks = pawnCaptureSquareMarks(sq1, side)
	    val aSqs = Square.foldMoveSquares(sq1, side, Piece.Pawn)(Set[Int](), true) { case ((_, b), sq) => randomMarks(sq) | (b | capMarks(sq)) } {
	      case ((sqs, _), sq) => (sqs + sq, false)
	    } {
	      case ((sqs, _), sq) => (sqs, true)
	    }._1
	    val eSqs = (0 to 63).filter { sq => mvMarks1(sq) | (mvMarks2(sq) & randomMarks(sq)) }.toSet
	    aSqs == eSqs
    }
  
  List((Piece.Knight, knightMoveSquareMarks), (Piece.King, kingMoveSquareMarks)).foreach { 
    case (piece, marksFun) => {
      property("foldNonSlidingMoveSquares for " + piece + " should return square set") =
        Prop.forAllNoShrink(squareGen) {
          (sq) =>
            val marks = marksFun(sq)
            val aSqs = Square.foldNonSlidingMoveSquares(sq, piece)(Set[Int]()) { (_, _) => true } { (sqs, sq) => if(marks(sq)) sqs + sq  else sqs }
            val eSqs = (0 to 63).filter { marks }.toSet
            aSqs == eSqs
        }
     
      property("foldMoveSquares for " + piece + " should return two  square sets") =
        Prop.forAllNoShrink(squareGen, sideGen, Gen.listOfN(64, Gen.value(false) | Gen.value(true))) {
          (sq, side, randomMarks) =>
            val marks = marksFun(sq)
            val (aSqs1, aSqs2) = Square.foldMoveSquares(sq, side, piece)(Set[Int](), Set[Int]()) { (_, sq) => randomMarks(sq) } {
              case ((sqs1, sqs2), sq) => (sqs1 + sq, sqs2)
            } {
              case ((sqs1, sqs2), sq) => (sqs1, sqs2 + sq)
            }
            val eSqs1 = (0 to 63).filter { sq => marks(sq) & randomMarks(sq) }.toSet
            val eSqs2 = (0 to 63).filter { sq => marks(sq) & !randomMarks(sq) }.toSet
            aSqs1 == eSqs1 && aSqs2 == eSqs2
        }
    }
  }
  
  List((Piece.Bishop, 4, bishopMoveSquareMarks), (Piece.Rook, 4, rookMoveSquareMarks), (Piece.Queen, 8, queenMoveSquareMarks)).foreach { 
    case (piece, nLimits, marksFun) => {
      property("foldSlidingMoveSquares for " + piece + " should return two square sets") =
        Prop.forAllNoShrink(squareGen, Gen.listOfN(nLimits, Gen.choose(0, 8))) {
          (sq, limits) =>
            val marks1 = marksFun(limits, sq)
            val marks2 = marksFun(limits.map { _  + 1 }, sq)
            val (aSqs1, aSqs2, aNum) = Square.foldSlidingMoveSquares(sq, piece)(Set[Int](), Set[Int](), 0) { _ => true } { 
              case (sqs1, sqs2, n) => (sqs1, sqs2, n + 1) 
            } { 
              case (_, sq) => marks1(sq) 
            } {
              case ((sqs1, sqs2, n), sq) => (sqs1 + sq, sqs2, n)
            } {
              case ((sqs1, sqs2, n), sq) => (sqs1, sqs2 + sq, n)
            }
            val eSqs1 = (0 to 63).filter { marks1 }.toSet
            val eSqs2 = (0 to 63).filter { sq => marks1(sq) ^ marks2(sq) }.toSet
            val eNum = nLimits
            aSqs1 == eSqs1 && aSqs2 == eSqs2 && aNum == eNum
        }
      
      property("foldMoveSquares for " + piece + " should return two  square sets") =
        Prop.forAllNoShrink(squareGen, sideGen, Gen.listOfN(nLimits, Gen.choose(0, 8))) {
          (sq, side, limits) =>
            val marks1 = marksFun(limits, sq)
            val marks2 = marksFun(limits.map { _  + 1 }, sq)
            val (aSqs1, aSqs2) = Square.foldMoveSquares(sq, side, piece)(Set[Int](), Set[Int]()) { (_, sq) => marks1(sq) } {
              case ((sqs1, sqs2), sq) => (sqs1 + sq, sqs2)
            } {
              case ((sqs1, sqs2), sq) => (sqs1, sqs2 + sq)
            }
            val eSqs1 = (0 to 63).filter { marks1 }.toSet
            val eSqs2 = (0 to 63).filter { sq => marks1(sq) ^ marks2(sq) }.toSet
            aSqs1 == eSqs1 && aSqs2 == eSqs2
        }
    }
  }
}