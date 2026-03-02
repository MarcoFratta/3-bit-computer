import computer.GuardedSequenceReader.*
import computer.SequenceReader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
class InputReaderTest extends AnyFlatSpec with Matchers:

  "The sequence input reader" should "correctly read values given a correct index" in {
    val input = "1,2,3"
    SequenceReader(input).read(4) match
      case Left(value) => fail()
      case Right(value) => value shouldEqual Some('3')
  }
  "The string -> conversion" should "work" in {
    val input = "-3,4,5,34".split(",")
    val reader = SequenceReader(input)
    reader.mapValues(_.toInt).read(2) match
      case Left(value) => fail()
      case Right(value) => value shouldBe Some(5)
  }

  "The input reader" should "fail values given a incorrect index" in {
    val input = "1,2,3".split(",")
    SequenceReader(input).mapValues(_.toInt).read(-1) match
      case Left(value) => value.getMessage should not be empty
      case Right(value) => fail()
  }

  "Attempting to read after all values have been red" should "not fail but return an empty Option" in {
    val input = "1,2,3".split(",")
    SequenceReader(input).mapValues(_.toInt).read(4) match
      case Left(value) => fail()
      case Right(value) => value shouldBe None
  }
  "Min value check" should "fail if a value is < min" in {
    val input = "1,2,3".split(",")
    val inputProgram = SequenceReader(input).mapValues(_.toInt).withGuards + min(3)
    inputProgram.read(1) match
      case Left(value) =>
      case Right(value) => fail(s"value was $value")
  }
  "Reading out of bounds" should "return None even with min check" in {
    val input = "1,2,3,4".split(",")
    val inputProgram = SequenceReader(input).mapValues(_.toInt).withGuards + min(6)
    inputProgram.read(5) match
      case Left(value) => fail()
      case Right(value) => value shouldBe None
  }
  "Max value check" should "fail if a value is > max" in {
    val input = "1,2,3".split(",")
    val inputProgram = SequenceReader(input).mapValues(_.toInt).withGuards + max(1)
    inputProgram.read(2) match
      case Left(value) =>
      case Right(value) => fail()
  }
  "Min and max value check" should "both work together" in {
    val input = "1,2,-1,6,7,8".split(",")
    val inputProgram = SequenceReader(input).mapValues(_.toInt).withGuards + min(0) + max(7)
    inputProgram.read(2) match
      case Left(value) =>
      case Right(value) => fail(s"Value was $value")
    inputProgram.read(5) match
      case Left(value) =>
      case Right(value) => fail()
    inputProgram.read(4) match
      case Left(value) => fail()
      case Right(value) => value shouldBe Some(7)
    inputProgram.read(9) match
      case Left(value) => fail()
      case Right(value) => value shouldBe None
  }