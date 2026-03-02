import GuardedSequenceReader.{max, min, withGuards}
import ThreeBitsState.State
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ThreeBitsState.state


class ComputerTest extends AnyFlatSpec with Matchers:

  private def testInputOutput(i:String)(s:State, errors: Boolean = false) =
    val reader = SequenceReader(i.trim.split(','))
      .mapValues(_.toInt).withGuards + min(0) + max(7)
    if errors then ThreeBitsComputer.withLoopDetection(s).compute(reader)
    else ThreeBitsComputer.ignoreErrors(s).compute(reader)

  "A computer program 1 " should "produce the correct output" in {
    val input =  "0,1,5,4,3,0"
    val initState = state(0,3729,0,0)
    testInputOutput(input)(initState, true) shouldEqual Seq(0,4,2,1,4,2,5,6,7,3,1,0).map(_.toString)
  }
  "A computer program 2 " should "produce the correct output" in {
    val input = "0,3,5,4,3,0"
    val initState = state(0, 8642024, 0, 0)
    testInputOutput(input)(initState, true) shouldEqual Seq(5,7,6,5,7,0,4,0).map(_.toString)
  }
  "A computer program " should "fail for an incorrect input" in {
    val input = "0,3,8,4,3,0"
    val initState = state(0, 8642024, 0, 0)
    testInputOutput(input)(initState,true).head shouldEqual "Values must be <= 7"
  }
  "A computer program" should "fail for a combo value (7)" in {
    val input = "0,7"
    val initState = state(0, 100, 0, 0)
    testInputOutput(input)(initState,true).head shouldEqual "7 is not a valid combo argument"
  }
  "A computer program" should "fail for a negative value (-2)" in {
    val input = "3,-2"
    val initState = state(0, 100, 0, 0)
    testInputOutput(input)(initState, true).head shouldEqual "Values must be >= 0"
  }
  "A computer program" should "fail for an odd number of inputs" in {
    val input = "3,0,3"
    val initState = state(2, 100, 0, 0)
    testInputOutput(input)(initState, true).head shouldEqual "Unexpected end of input, expected operand at 3"
  }
  "A computer program" should "fail if it detects a loop" in {
    val input = "3,0,3"
    val initState = state(0, 100, 0, 0)
    testInputOutput(input)(initState, true).head shouldEqual "Loop detected at 0"
  }
  "A computer program " should "detect loop if has a jump to the start at the end" in {
    val input = "0,1,5,4,2,0,3,2"
    val initState = state(0, 3729, 0, 0)
    testInputOutput(input)(initState, true).last shouldEqual "Loop detected at 2"
  }
  "A computer program" should "handle a correct loop value" in {
    val input = "5,4,0,1,3,0,5,4"
    val initState = state(0, 10, 0, 0)

    testInputOutput(input)(initState, true) shouldEqual Seq(2,5,2,1,0).map(_.toString)
  }
  "A computer program " should "detect loop that update registry before jumping back " in {
    val input = " 1,7,1,7,3,0"
    val initState = state(0, 1, 0, 0)
    testInputOutput(input)(initState, true).last shouldEqual "Loop detected at 2"
  }
  "A computer program " should "detect loops that are longer than 1" in {
    val input = " 1,1,3,0"
    val initState = state(0, 10, 0, 0)
    testInputOutput(input)(initState, true).last shouldEqual "Loop detected at 2"
  }
  "A computer program " should "detect loops that are occurs at the start state" in {
    val input = " 1,1,3,0"
    val initState = state(0, 1, 0, 0)
    testInputOutput(input)(initState, true).last shouldEqual "Loop detected at 2"
  }


