package threeBits

import computer.*
import computer.Operands.withOverrides
import computer.RegistryOps.*
import threeBits.ThreeBitsInstruction.*
import threeBits.ThreeBitsState.Registry.*
import threeBits.ThreeBitsState.{*, given}
import computer.Operands.literal

object ThreeBitsComputer:
  val bits = 3
  // we use operands api to create our combo representation
  val threeBitCombo: ValueType => Operand[State, ValueType] = withOverrides[State, ValueType](
    4 -> (_(X)),
    5 -> (_(Y)),
    6 -> (_(Z)),
    7 -> { _ => throw IllegalArgumentException("7 is not a valid combo argument") }
  )
  // This design allows for reusing operations even for
  // computer with different architectures (4 bits, 5 bits ...)
  val instructions: Map[Int, Instruction[State, ValueType, ValueType]] = Seq(
    xdv(threeBitCombo), yxl(literal),
    yst(bits, threeBitCombo), jnz(literal),
    yxz, out(bits, threeBitCombo),
    ydv(threeBitCombo), zdv(threeBitCombo))
    .zipWithIndex
    .map(_.reverse).toMap
  

  def withLoopDetection: State => Computer[ValueType, Seq[String]] = 
    SingleOperandComputer.withLoopDetection(instructions)
  def withErrors: State => Computer[ValueType, Seq[String]] = 
    SingleOperandComputer.withErrors(instructions)
  def ignoreErrors: State => Computer[ValueType, Seq[ValueType]] =
    SingleOperandComputer.ignoreErrors(instructions)
  
      
    
  
  
    



