import Operands.identity
import SingleOperandInstructions.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InstructionsFactoryTest extends AnyFlatSpec with Matchers:
  private val factory = ThreeBitsComputer
  "Get for three bits" should "return 8 operations" in {
    factory.getForThreeBits.size shouldBe 8
  }
  "OpCodes" should "be correct for each instruction" in {
    val map = factory.getForThreeBits.map(x => x._1 -> x._2.getClass)
    map(0) shouldEqual Xdv(identity).getClass
    map(1) shouldEqual Yxl(identity).getClass
    map(2) shouldEqual Yst(3, identity).getClass
    map(3) shouldEqual Jnz(identity).getClass
    map(4) shouldEqual Yxz().getClass
    map(5) shouldEqual Out(3, identity).getClass
    map(6) shouldEqual Ydv(identity).getClass
    map(7) shouldEqual Zdv(identity).getClass
  }
