import Computer.State
import Operand.{combo, literal}

import scala.math.pow

trait Instruction[T]:
  val opcode: Int

  def apply(s: State, operand: T): (State, Option[T])

object Operations:

  class Xdv extends Instruction[Int]:
    override val opcode: Int = 0

    def apply(s: State, operand: Int): (State, Option[Int]) =
      s.copy(x = (s.x / pow(2, combo(operand).get(s))).intValue()) -> Option.empty


  class Yxl extends Instruction[Int]:
    override val opcode: Int = 1

    override def apply(s: State, operand: Int): (State, Option[Int]) =
      s.copy(y = s.y ^ literal(operand).get(s)) -> Option.empty

  class Yst extends Instruction[Int]:
    override val opcode: Int = 2

    override def apply(s: State, operand: Int): (State, Option[Int]) =
      s.copy(y = combo(operand).get(s) % 8) -> Option.empty

  class Jnz extends Instruction[Int]:
    override val opcode: Int = 3

    override def apply(s: State, operand: Int): (State, Option[Int]) = s.x match
      case 0 => s -> Option.empty
      case _ => s.copy(ip = literal(operand).get(s)) -> Option.empty

  class Yxz extends Instruction[Int]:
    override val opcode: Int = 4

    override def apply(s: State, operand: Int): (State, Option[Int]) =
      s.copy(y = s.y ^ s.z) -> Option.empty

  class Out extends Instruction[Int]:
    override val opcode: Int = 5

    override def apply(s: State, operand: Int): (State, Option[Int]) =
      s -> Option(combo(operand).get(s) % 8)

  class Ydv extends Instruction[Int]:
    override val opcode: Int = 6

    override def apply(s: State, operand: Int): (State, Option[Int]) =
      val op = Xdv()
      s.copy(y = op.apply(s, operand)._1.x) -> Option.empty

  class Zdv extends Instruction[Int]:
    override val opcode: Int = 7

    override def apply(s: State, operand: Int): (State, Option[Int]) =
      val op = Xdv()
      s.copy(z = op.apply(s, operand)._1.x) -> Option.empty




  
  
  
  