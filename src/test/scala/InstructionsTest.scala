import Computer.State
import Operands.*
import SingleOperandInstructions.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InstructionsTest extends AnyFlatSpec with Matchers:

  private val testCombo = withOverrides(
    4 -> (_.x),
    5 -> (_.y),
    6 -> (_.z),
    7 -> (_ => throw IllegalArgumentException("7 is not a valid combo argument")))
  // opCode 0
  "xdv(x = 10, operand = 2) " should "set x = 10/4 = 2.5 = 2" in {
    val instruction = Xdv(testCombo) 
    val s = State(0,10,0,0)
    instruction(s, 2)._1.x shouldBe 2
    instruction(s, 2)._1.ip shouldBe 2
  }
  "xdv(x = 1024, operand = 6 (z = 9) " should "set x = 1024/2^9 = 2" in {
    val instruction = Xdv(testCombo)
    val s = State(0, 1024, 0, 9)
    instruction(s, 6)._1.x shouldBe 2
  }

  // opCode 1
  "yxl(y = 2, operand = 5)" should "set y = 7" in {
    val instruction = Yxl(identity)
    val s = State(0, 0, 2, 0)
    instruction(s, 5)._1.y shouldBe 7
  }

  "yxl(y = 0, operand = 0)" should "set y = 0" in {
    val instruction = Yxl(identity)
    val s = State(0, 0, 0, 0)
    instruction(s, 0)._1.y shouldBe 0
  }

  // opCode 2
  "yst(operand = 6 -> (z=8))" should "set y = 0 (8 % 8 = 0)" in {
    val instruction = Yst(3, testCombo)
    val s = State(0, 0, 0, 8)
    instruction(s, 6)._1.y shouldBe 0
  }

  "yst(operand = 3))" should "set y = 0 (3 % 8 = 3)" in {
    val instruction = Yst(3, testCombo)
    val s = State(0, 0, 0, 0)
    instruction(s, 3)._1.y shouldBe 3
  }
  // opCode 3
  "jnz with x = 0" should "not jump" in {
    val instruction = Jnz(identity)
    val s = State(0, 0, 0, 0)
    instruction(s, 5)._1 should not be 5
  }

  "jnz(operand=5) with x = 12" should "jump ip to 5" in {
    val instruction = Jnz(identity)
    val s = State(1, 12, 0, 0)
    instruction(s, 5)._1.ip shouldEqual 5
  }

  // opCode 4
  "yxz(y = 3, z = 1 ) " should "set y = 2" in {
//    y =   0 1 1
//    z =   0 0 1
//    xor = 0 1 0 = 2
    val instruction = Yxz()
    val s = State(0, 0, 3, 1)
    instruction(s, 30)._1.y shouldEqual 2
  }
  // opCode 5
  "out(operand = 3)" should "output 3" in {
    val instruction = Out(3, testCombo)
    val s = State(0, 0, 3, 1)
    instruction(s, 3)._2 shouldEqual Option(3)
  }

  "out(operand = 6) z = 24" should "output 0" in {
    val instruction = Out(3, testCombo)
    val s = State(0, 0, 0, 24)
    instruction(s, 6)._2 shouldEqual Option(0)
  }

  // opCode 6
  "xdv(x = 16, operand = 3) " should "set y = 16/8 = 2" in {
    val instruction = Ydv(testCombo)
    val s = State(0, 16, 0, 0)
    instruction(s, 3)._1.y shouldBe 2
  }

  // opCode 7
  "xdv(x = 16, operand = 3) " should "set z = 16/8 = 2" in {
    val instruction = Zdv(testCombo)
    val s = State(0, 16, 0, 0)
    instruction(s, 3)._1.z shouldBe 2
  }

   "all instructions except jnz" should "increase ip by 2" in {
     val instructions = List(Xdv(testCombo), Yxl(identity), Yst(3, testCombo), Yxz(), Out(3, testCombo),
       Ydv(testCombo), Zdv(testCombo))
     val s = State(0,0,0,0)
     instructions foreach (
       f => f(s, 0)._1.ip shouldBe 2
     )
   }
   "jnz, when x = 0" should "increase by 2 the ip" in {
     val instruction = Jnz(identity)
     val s = State(0,0,0,0)
     instruction(s, 2)._1.ip shouldBe 2
   }

