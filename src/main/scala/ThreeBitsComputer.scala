import Computer.State
import Operands.*
import SingleOperandInstructions.*

object ThreeBitsComputer:
  private type OpCode = Int
  private val threeBitCombo = withOverrides(
    4 -> (_.x),
    5 -> (_.y),
    6 -> (_.z),
    7 -> {_ => throw IllegalArgumentException("7 is not a valid combo argument")}
  )
  // This design allows for reusing operations even for
  // computer with different architectures (4 bits, 5 bits ...)
  val getForThreeBits: Map[OpCode, Instruction[Int]] =
    val bits = 3
    Seq(Xdv(threeBitCombo), Yxl(identity),
      Yst(bits, threeBitCombo), Jnz(identity),
      Yxz(), Out(bits, threeBitCombo),
      Ydv(threeBitCombo), Zdv(threeBitCombo))
      .zipWithIndex
      .map(_.reverse)
      .toMap



