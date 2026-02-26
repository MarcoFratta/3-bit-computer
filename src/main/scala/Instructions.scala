import Computer.State
import Operand.{combo, literal}

import scala.math.pow

trait Instruction[T] extends ((State, T) => (State, Option[T]))

trait IpIncrement[T](step: Int) extends Instruction[T]:
  abstract override def apply(s: State, operand: T): (State, Option[T]) =
    val (s2, o) = super.apply(s, operand)
    s2.copy(ip = s2.ip + step) -> o

abstract class BasicOperation[T](f: (State, T) => (State, Option[T])) extends Instruction[T]:
  override def apply(s: State, operand: T): (State, Option[T]) = f(s, operand)

object Operations:
  // IP incrementation (by default) depends on the number of operands needed by the instruction
  private val OPERANDS_NUMBER = 2
  opaque type Instruction = BasicOperation[Int]

  class Xdv extends Instruction((s, operand) =>
    s.copy(x = (s.x / pow(2, combo(operand).get(s))).intValue) -> None)
    with IpIncrement[Int](OPERANDS_NUMBER) 

  class Yxl extends Instruction((s, operand) =>
    s.copy(y = s.y ^ literal(operand).get(s)) -> None)
    with IpIncrement[Int](OPERANDS_NUMBER) 

  class Yst(bits: Int) extends Instruction((s, operand) =>
    s.copy(y = combo(operand).get(s) % pow(2, bits).intValue) -> None)
    with IpIncrement[Int](OPERANDS_NUMBER)

  class Jnz extends Instruction((s, operand) => s.x match
    case 0 => s.copy(ip = s.ip + 2) -> None
    case _ => s.copy(ip = literal(operand).get(s)) -> None)
  
  class Yxz extends Instruction((s, operand) =>
    s.copy(y = s.y ^ s.z) -> None)
    with IpIncrement[Int](OPERANDS_NUMBER) 

  class Out(bits: Int) extends Instruction((s, operand) =>
    s -> Some(combo(operand).get(s) % pow(2, bits).intValue))
    with IpIncrement[Int](OPERANDS_NUMBER) 

  class Ydv extends Instruction((s, operand) =>
    s.copy(y = new Xdv().apply(s, operand)._1.x) -> None)
    with IpIncrement[Int](OPERANDS_NUMBER) 

  class Zdv extends Instruction((s, operand) =>
    s.copy(z = new Xdv().apply(s, operand)._1.x) -> None)
    with IpIncrement[Int](OPERANDS_NUMBER) 






  
  
  
  