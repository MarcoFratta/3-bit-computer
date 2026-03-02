import Operands.withOverrides
import RegistryOps.*
import ThreeBitsInstruction.*
import ThreeBitsState.Registry.*
import ThreeBitsState.*
import Operands.*
import ThreeBitsState.given

object ThreeBitsComputerFacade:
  val threeBitCombo: ValueType => Operand[State, ValueType] = withOverrides[State, ValueType](
    4 -> (_(X)),
    5 -> (_(Y)),
    6 -> (_(Z)),
    7 -> {_ => throw IllegalArgumentException("7 is not a valid combo argument")}
  )
  private val literal = identity[State, ValueType]
  // This design allows for reusing operations even for
  // computer with different architectures (4 bits, 5 bits ...)
  val bits = 3
  val instructions: Map[Int, Instruction[State, ValueType, ValueType]] = Seq(xdv(threeBitCombo), yxl(literal),
      yst(bits, threeBitCombo), jnz(literal),
      yxz, out(bits, threeBitCombo),
      ydv(threeBitCombo), zdv(threeBitCombo))
      .zipWithIndex
      .map(_.reverse).toMap
  
    def apply(logErrors:Boolean=false): State => Computer[ValueType, ? >: Seq[String] & Seq[ValueType] <: Seq[Any]] = 
      if logErrors 
        then SingleOperandComputer.withLoopDetection(instructions)
      else SingleOperandComputer.ignoreErrors(instructions)
      
    
  
  
    



