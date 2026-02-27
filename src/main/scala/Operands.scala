import Computer.State

trait Operands[T] extends (State => T)

object IntOperands:
  type OperandPolicy = Int => Operands[Int]
  def literal(value: Int): Operands[Int] = _ => value
  def combo(overrides: Map[Int, Operands[Int]])(value: Int): Operands[Int] = value match
    case n if overrides isDefinedAt n => overrides(n)
    case n => literal(n)
