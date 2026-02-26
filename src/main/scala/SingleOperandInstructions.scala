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

object SingleOperandInstructions:
  // IP incrementation (by default) depends on the number of operands needed by the instruction
  private val OPERANDS_NUMBER = 2

  sealed case class SingleOpInstruction[T](f: (State, T) => (State, Option[T]))
    extends BasicOperation[T](f)
    with IpIncrement[T](OPERANDS_NUMBER)
  
  class Xdv extends SingleOpInstruction[Int]((s, operand) =>
    s.copy(x = (s.x / pow(2, combo(operand).get(s))).intValue) -> None)

  class Yxl extends SingleOpInstruction[Int]((s, operand) =>
    s.copy(y = s.y ^ literal(operand).get(s)) -> None)

  class Yst(bits: Int) extends SingleOpInstruction[Int]((s, operand) =>
    s.copy(y = combo(operand).get(s) % pow(2, bits).intValue) -> None)

  class Jnz extends BasicOperation[Int]((s, operand) => s.x match
    case 0 => s.copy(ip = s.ip + 2) -> None
    case _ => s.copy(ip = literal(operand).get(s)) -> None)

  class Yxz extends SingleOpInstruction[Int]((s, operand) => s.copy(y = s.y ^ s.z) -> None)

  class Out(bits: Int) extends SingleOpInstruction[Int]((s, operand) =>
    s -> Some(combo(operand).get(s) % pow(2, bits).intValue))

  class Ydv extends SingleOpInstruction[Int]((s, operand) =>
    s.copy(y = new Xdv().apply(s, operand)._1.x) -> None)

  class Zdv extends SingleOpInstruction[Int]((s, operand) =>
    s.copy(z = new Xdv().apply(s, operand)._1.x) -> None)






  
  
  
  