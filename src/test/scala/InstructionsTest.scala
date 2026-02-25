import Computer.State
import Operations.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InstructionsTest extends AnyFlatSpec with Matchers:

  // OPCODE 0
  "xdv(x = 10, operand = 2) " should "set x = 10/4 = 2.5 = 2" in {
    val instruction = Xdv()
    instruction.opcode shouldBe 0
    val s = State(0,10,0,0)
    instruction(s, 2)._1.x shouldBe 2
  }
  "xdv(x = 1024, operand = 6 (z = 9) " should "set x = 1024/2^9 = 2" in {
    val instruction = Xdv()
    val s = State(0, 1024, 0, 9)
    instruction(s, 6)._1.x shouldBe 2
  }

  // OPCODE 1
  "yxl(y = 2, operand = 5)" should "set y = 7" in {
    val instruction = Yxl()
    instruction.opcode shouldBe 1
    val s = State(0, 0, 2, 0)
    instruction(s, 5)._1.y shouldBe 7
  }

  "yxl(y = 0, operand = 0)" should "set y = 0" in {
    val instruction = Yxl()
    val s = State(0, 0, 0, 0)
    instruction(s, 0)._1.y shouldBe 0
  }

  // OPCODE 2
  "yst(operand = 6 -> (z=8))" should "set y = 0 (8 % 8 = 0)" in {
    val instruction = Yst()
    instruction.opcode shouldBe 2
    val s = State(0, 0, 0, 8)
    instruction(s, 6)._1.y shouldBe 0
  }

  "yst(operand = 3))" should "set y = 0 (3 % 8 = 3)" in {
    val instruction = Yst()
    val s = State(0, 0, 0, 0)
    instruction(s, 3)._1.y shouldBe 3
  }
  // OPCODE 3
  "jnz with x = 0" should "do nothing" in {
    val instruction = Jnz()
    instruction.opcode shouldBe 3
    val s = State(0, 0, 0, 0)
    instruction(s, 5)._1 shouldEqual s
  }

  "jnz(operand=5) with x = 12" should "jump ip to 5" in {
    val instruction = Jnz()
    val s = State(1, 12, 0, 0)
    instruction(s, 5)._1.ip shouldEqual 5
  }

  // OPCODE 4
  "yxz(y = 3, z = 1 ) " should "set y = 2" in {
//    y =   0 1 1
//    z =   0 0 1
//    xor = 0 1 0 = 2
    val instruction = Yxz()
    instruction.opcode shouldBe 4
    val s = State(0, 0, 3, 1)
    instruction(s, 30)._1.y shouldEqual 2
  }
  // OPCODE 5
  "out(operand = 3)" should "output 3" in {
    val instruction = Out()
    instruction.opcode shouldBe 5
    val s = State(0, 0, 3, 1)
    instruction(s, 3)._2 shouldEqual Option(3)
  }

  "out(operand = 6) z = 24" should "output 0" in {
    val instruction = Out()
    instruction.opcode shouldBe 5
    val s = State(0, 0, 0, 24)
    instruction(s, 6)._2 shouldEqual Option(0)
  }

  // OPCODE 6
  "xdv(x = 16, operand = 3) " should "set y = 16/8 = 2" in {
    val instruction = Ydv()
    instruction.opcode shouldBe 6
    val s = State(0, 16, 0, 0)
    instruction(s, 3)._1.y shouldBe 2
  }

  // OPCODE 7
  "xdv(x = 16, operand = 3) " should "set z = 16/8 = 2" in {
    val instruction = Zdv()
    instruction.opcode shouldBe 7
    val s = State(0, 16, 0, 0)
    instruction(s, 3)._1.z shouldBe 2
  }

