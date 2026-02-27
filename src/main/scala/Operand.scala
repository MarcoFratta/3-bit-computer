import Computer.State

trait Operand[T] extends (State => T)

object Operands:
  type OperandPolicy[T] = T => Operand[T]
  def identity[T](value: T): Operand[T] = _ => value
  def withOverride[T](ov: (T, Operand[T]))(value:T):Operand[T] = withOverrides(Seq(ov)*)(value)
  def withOverrides[T](overrides: (T,Operand[T])*)(value:T): Operand[T] = withOverrides(overrides.toMap)(value)
  def withOverrides[T](overrides: Map[T, Operand[T]])(value: T): Operand[T] = value match
    case n if overrides isDefinedAt n => overrides(n)
    case n => identity(n)
