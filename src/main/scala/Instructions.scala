import Computer.State
import Operand.{combo, literal}

import scala.math.pow

trait Instruction[T]:
  val opCode: Int

  def apply(s: State, operand: T): (State, Option[T])

trait OpCode[T](oc: Int) extends Instruction[T]:
  override val opCode: Int = oc

trait IpIncrement[T](step: Int) extends Instruction[T]:
  abstract override def apply(s: State, operand: T): (State, Option[T]) =
    val (s2, o) = super.apply(s, operand)
    s2.copy(ip = s2.ip + step) -> o

abstract class BasicOperation[T](f: (State, T) => (State, Option[T])) extends Instruction[T]:
  override def apply(s: State, operand: T): (State, Option[T]) = f(s, operand)

object Operations:
  opaque type Instruction = BasicOperation[Int]

  class Xdv extends Instruction((s, operand) =>
    s.copy(x = (s.x / pow(2, combo(operand).get(s))).intValue()) -> Option.empty)
    with IpIncrement[Int](2) with OpCode[Int](0)

  class Yxl extends Instruction((s, operand) =>
    s.copy(y = s.y ^ literal(operand).get(s)) -> Option.empty)
    with IpIncrement[Int](2) with OpCode[Int](1)

  class Yst extends Instruction((s, operand) =>
    s.copy(y = combo(operand).get(s) % 8) -> Option.empty)
    with IpIncrement[Int](2) with OpCode[Int](2)

  class Jnz extends Instruction((s, operand) => s.x match
    case 0 => s.copy(ip = s.ip + 2) -> Option.empty
    case _ => s.copy(ip = literal(operand).get(s)) -> Option.empty)
    with OpCode[Int](3)

  class Yxz extends Instruction((s, operand) =>
    s.copy(y = s.y ^ s.z) -> Option.empty)
    with IpIncrement[Int](2) with OpCode[Int](4)

  class Out extends Instruction((s, operand) =>
    s -> Option(combo(operand).get(s) % 8))
    with IpIncrement[Int](2) with OpCode[Int](5)

  class Ydv extends Instruction((s, operand) =>
    s.copy(y = new Xdv().apply(s, operand)._1.x) -> Option.empty)
    with IpIncrement[Int](2) with OpCode[Int](6)

  class Zdv extends Instruction((s, operand) =>
    s.copy(z = new Xdv().apply(s, operand)._1.x) -> Option.empty)
    with IpIncrement[Int](2) with OpCode[Int](7)






  
  
  
  