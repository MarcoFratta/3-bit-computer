
object Operands:
  // We choose which operand we want to use based on the value of the operand
  private type OperandPolicy[S, R] = (R, Operand[S, R])

  def withOverride[S, R](ov: OperandPolicy[S, R])(value: R): Operand[S, R] = withOverrides(Seq(ov) *)(value)

  // we can create custom logic where a certain value, represents
  // a pointer to an internal state value, instead of its literal value
  def withOverrides[S, R](overrides: OperandPolicy[S, R]*)(value: R): Operand[S, R] = fromMap(overrides.toMap)(value)

  private def fromMap[S, R](overrides: Map[R, Operand[S, R]])(value: R): Operand[S, R] = value match
    case n if overrides isDefinedAt n => overrides(n)
    case n => identity(n)

  def identity[S, R](value: R): Operand[S, R] = _ => value
