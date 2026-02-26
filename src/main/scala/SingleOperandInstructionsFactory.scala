import SingleOperandInstructions.*

object SingleOperandInstructionsFactory:
  private type OpCode = Int
  // This design allows for reusing operations even for
  // computer with different architectures (4 bits, 5 bits ...)
  val getForThreeBits: Map[OpCode, Instruction[Int]] =
    val bits = 3
    Seq(Xdv(), Yxl(), Yst(bits), Jnz(), Yxz(), Out(bits), Ydv(), Zdv())
      .zipWithIndex
      .map(_.reverse)
      .toMap



