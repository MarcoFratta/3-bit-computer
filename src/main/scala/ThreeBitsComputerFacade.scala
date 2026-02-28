import Operands.*
import SingleOperandComputer.State
import SingleOperandInstructions.*

object ThreeBitsComputerFacade:
  private type OpCode = Int
  private val threeBitCombo = withOverrides(
    4 -> (_.x),
    5 -> (_.y),
    6 -> (_.z),
    7 -> {_ => throw IllegalArgumentException("7 is not a valid combo argument")}
  )
  private val literal = identity[Int]
  // This design allows for reusing operations even for
  // computer with different architectures (4 bits, 5 bits ...)
  val bits = 3
  val instructions: Map[OpCode, Instruction[Int]] = Seq(Xdv(threeBitCombo), Yxl(literal),
      Yst(bits, threeBitCombo), Jnz(literal),
      Yxz(), Out(bits, threeBitCombo),
      Ydv(threeBitCombo), Zdv(threeBitCombo))
      .zipWithIndex
      .map(_.reverse).toMap
  
    def apply(logErrors:Boolean=false): State => Computer[OpCode, ? >: Seq[String] & Seq[OpCode] <: Seq[Any]] = 
      if logErrors 
        then SingleOperandComputer.withLoopDetection(instructions)
      else SingleOperandComputer.ignoreErrors(instructions)
      
    
  
  
    



