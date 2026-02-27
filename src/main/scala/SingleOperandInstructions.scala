import Computer.State
import Operands.OperandPolicy

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
  private val IP_STEP_NUMBER = 2
  
  sealed case class SingleOpInstruction[T](f: (State, T) => (State, Option[T]))
    extends BasicOperation[T](f)
    with IpIncrement[T](IP_STEP_NUMBER)
  
  class Xdv(getter: OperandPolicy[Int]) extends SingleOpInstruction[Int]((s, operand) =>
    s.copy(x = (s.x / pow(2, getter(operand)(s))).intValue) -> None)

  class Yxl(getter: OperandPolicy[Int]) extends SingleOpInstruction[Int]((s, operand) =>
    s.copy(y = s.y ^ getter(operand)(s)) -> None)

  class Yst(bits: Int, getter: OperandPolicy[Int]) extends SingleOpInstruction[Int]((s, operand) =>
    s.copy(y = getter(operand)(s) % pow(2, bits).intValue) -> None)

  class Jnz(getter: OperandPolicy[Int]) extends BasicOperation[Int]((s, operand) => s.x match
    case 0 => s.copy(ip = s.ip + IP_STEP_NUMBER) -> None
    case _ => s.copy(ip = getter(operand)(s)) -> None)

  class Yxz extends SingleOpInstruction[Int]((s, operand) => s.copy(y = s.y ^ s.z) -> None)

  class Out(bits: Int, getter: OperandPolicy[Int]) extends SingleOpInstruction[Int]((s, operand) =>
    s -> Some(getter(operand)(s) % pow(2, bits).intValue))

  class Ydv(getter: OperandPolicy[Int]) extends SingleOpInstruction[Int]((s, operand) =>
    s.copy(y = new Xdv(getter).apply(s, operand)._1.x) -> None)

  class Zdv(getter: OperandPolicy[Int]) extends SingleOpInstruction[Int]((s, operand) =>
    s.copy(z = new Xdv(getter).apply(s, operand)._1.x) -> None)






  
  
  
  