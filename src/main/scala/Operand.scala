trait Operand[S, R] extends (S => R)

object Operands:
  // We choose which operand we want to use based on the value of the operand
  type OperandPolicy[S, R] = (R, Operand[S, R])
  def identity[S, R](value: R): Operand[S, R] = _ => value
  def withOverride[S, R](ov: OperandPolicy[S, R])(value:R):Operand[S, R] = withOverrides(Seq(ov)*)(value)
  def withOverrides[S, R](overrides: OperandPolicy[S, R]*)(value:R): Operand[S, R] = fromMap(overrides.toMap)(value)
  
  private def fromMap[S, R](overrides: Map[R, Operand[S, R]])(value: R): Operand[S, R] = value match
    case n if overrides isDefinedAt n => overrides(n)
    case n => identity(n)
