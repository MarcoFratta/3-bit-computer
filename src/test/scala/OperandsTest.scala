import Operands.*
import RegistryOps.*
import ThreeBitsComputerFacade.threeBitCombo
import ThreeBitsState.Registry.*
import ThreeBitsState.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class OperandsTest extends AnyFlatSpec with Matchers:

  private val s = state(0,15,16,17)
  "a literal operand between 0 and 7" must " return that value" in {
    List.range(0, 8)
      .map(identity)
      .map(_(s))
      .zipWithIndex
      .forall((i, v) => i.equals(v)) mustBe true
  }

  "a combo override" must "work" in {
    val testCombo = withOverride(2 -> (_ => 3))
    testCombo(2)(s) mustBe 3
  }

  "a combo operand between 0 and 3" must "return that value" in {
    List.range(0, 4)
      .map(threeBitCombo)
      .map(_(s))
      .zipWithIndex
      .forall((i, v) => i.equals(v)) mustBe true
  }
  "a combo operand with 4 value" must "return x registry value" in {
    threeBitCombo(4)(s) mustBe s(X)
  }
  "a combo operand with 5 value" must "return y registry value" in {
    threeBitCombo(5)(s) mustBe s(Y)
  }
  "a combo operand with 6 value" must "return z registry value" in {
    threeBitCombo(6)(s) mustBe s(Z)
  }
  "a combo greater or equal than 7 " must "throw an error" in {
    an[IllegalArgumentException] must be thrownBy threeBitCombo(7)(s)
  }
