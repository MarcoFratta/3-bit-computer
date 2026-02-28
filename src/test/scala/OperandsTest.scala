import Operands.*
import SingleOperandComputer.State
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class OperandsTest extends AnyFlatSpec with Matchers:

  val state = State(0,15,16,17)
  val threeBitCombo = withOverrides(4 -> (_.x), 5 -> (_.y), 6 -> (_.z),
    7 -> {_ => throw IllegalArgumentException("7 is not valid")})
  "a literal operand between 0 and 7" must " return that value" in {
    List.range(0, 8)
      .map(identity)
      .map(_(state))
      .zipWithIndex
      .forall((i, v) => i.equals(v)) mustBe true
  }

  "a combo override" must "work" in {
    val testCombo = withOverride(2 -> (_ => 3))
    testCombo(2)(state) mustBe 3
  }

  "a combo operand between 0 and 3" must "return that value" in {
    List.range(0, 4)
      .map(threeBitCombo)
      .map(_(state))
      .zipWithIndex
      .forall((i, v) => i.equals(v)) mustBe true
  }
  "a combo operand with 4 value" must "return x registry value" in {
    threeBitCombo(4)(state) mustBe state.x
  }
  "a combo operand with 5 value" must "return y registry value" in {
    threeBitCombo(5)(state) mustBe state.y
  }
  "a combo operand with 6 value" must "return z registry value" in {
    threeBitCombo(6)(state) mustBe state.z
  }
  "a combo greater or equal than 7 " must "throw an error" in {
    an[IllegalArgumentException] must be thrownBy threeBitCombo(7)(state)
  }
