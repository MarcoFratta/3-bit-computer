import Computer.State
import Operand.{combo, literal}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class OperandsTest extends AnyFlatSpec with Matchers:

  val state = State(15,16,17)
  "a literal operand between 0 and 7" must " return that value" in {
    List.range(0, 8)
      .map(literal)
      .map(_.get(state))
      .zipWithIndex
      .forall((i, v) => i.equals(v)) mustBe true
  }
  "a negative literal operand " must "throw an error" in {
    an [IllegalArgumentException] must be thrownBy literal(-2)
  }
  "a literal greater than 7 " must "throw an error" in {
    an [IllegalArgumentException] must be thrownBy literal(8)
  }

  "a combo operand between 0 and 3" must "return that value" in {
    List.range(0, 4)
      .map(combo)
      .map(_.get(state))
      .zipWithIndex
      .forall((i, v) => i.equals(v)) mustBe true
  }
  "a combo operand with 4 value" must "return x registry value" in {
    combo(4).get(state) mustBe state.x
  }
  "a combo operand with 5 value" must "return y registry value" in {
    combo(5).get(state) mustBe state.y
  }
  "a combo operand with 6 value" must "return z registry value" in {
    combo(6).get(state) mustBe state.z
  }
  "a negative combo operand " must "throw an error" in {
    an[IllegalArgumentException] must be thrownBy combo(-2)
  }
  "a combo greater or equal than 7 " must "throw an error" in {
    an[IllegalArgumentException] must be thrownBy combo(7)
  }
