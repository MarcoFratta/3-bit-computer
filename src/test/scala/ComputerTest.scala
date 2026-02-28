import GuardedSequenceReader.{max, min, withGuards}
import SingleOperandComputer.State
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ComputerTest extends AnyFlatSpec with Matchers:

  private def testInputOutput(i:String)(s:State, errors: Boolean = false) =
    val reader = SequenceReader(i.split(','))
      .mapValues(_.toInt).withGuards + min(0) + max(7)
    ThreeBitsComputerFacade(errors)(s).compute(reader)

  "A computer program 1 " should "produce the correct output" in {
    val input =  "0,1,5,4,3,0"
    val initState = State(0,3729,0,0)
    testInputOutput(input)(initState) shouldEqual Seq(0,4,2,1,4,2,5,6,7,3,1,0)
  }
  "A computer program 2 " should "produce the correct output" in {
    val input = "0,3,5,4,3,0"
    val initState = State(0, 8642024, 0, 0)
    testInputOutput(input)(initState) shouldEqual Seq(5,7,6,5,7,0,4,0)
  }
  "A computer program " should "fail for an incorrect input" in {
    val input = "0,3,8,4,3,0"
    val initState = State(0, 8642024, 0, 0)
    testInputOutput(input)(initState,true).head shouldEqual "Values must be <= 7"
  }
  "A computer program" should "fail for a combo value (7)" in {
    val input = "0,7"
    val initState = State(0, 100, 0, 0)
    testInputOutput(input)(initState,true).head shouldEqual "7 is not a valid combo argument"
  }
  "A computer program" should "fail for a negative value (-2)" in {
    val input = "3,-2"
    val initState = State(0, 100, 0, 0)
    testInputOutput(input)(initState, true).head shouldEqual "Values must be >= 0"
  }
//  "A computer program" should "fail for an odd number of inputs" in {
//    val input = "3,0,3"
//    val initState = State(0, 100, 0, 0)
//    testInputOutput(input)(initState, true).head shouldEqual "Unex"
//  }
