import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import IntInputReader.*
class InputReaderTest extends AnyFlatSpec with Matchers:

  "The string -> conversion" should "work" in {
    val input = "-3,4,5,34"
    input.toIntValues shouldEqual Seq(-3,4,5,34)
  }

  "The program input" should "correctly read values given a correct index" in {
    val input = "1,2,3"
    Builder(input.toIntValues).build().read(2) match
      case Left(value) => fail()
      case Right(value) => value shouldEqual Some(3)
  }

  "The program input" should "fail values given a incorrect index" in {
    val input = "1,2,3"
    Builder(input.toIntValues).build().read(-1) match
      case Left(value) => value.getMessage should not be empty
      case Right(value) => fail()
  }

  "Attempting to read after all values have been red" should "not fail but return an empty Option" in {
    val input = "1,2,3"
    Builder(input.toIntValues).build().read(4) match
      case Left(value) => fail()
      case Right(value) => value shouldBe None
  }
  "Min value check" should "fail if a value is < min" in {
    val input = "1,2,3"
    val inputProgram = Builder(input.toIntValues).min(3).build()
    inputProgram.read(1) match
      case Left(value) =>
      case Right(value) => fail()
  }
  "Reading out of bounds" should "return None even with min check" in {
    val input = "1,2,3,4"
    val inputProgram = Builder(input.toIntValues).min(3).build()
    inputProgram.read(5) match
      case Left(value) => fail()
      case Right(value) => value shouldBe None
  }
  "Max value check" should "fail if a value is > max" in {
    val input = "1,2,3"
    val inputProgram = Builder(input.toIntValues).max(1).build()
    inputProgram.read(2) match
      case Left(value) =>
      case Right(value) => fail()
  }
  "Min and max value check" should "both work together" in {
    val input = "1,2,-1,6,7,8"
    val inputProgram = Builder(input.toIntValues).min(0).max(7).build()
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