import Computer.State

trait Operand:
  def get(s: State): Int

object Operand:
  private case class Literal(value: Int) extends Operand:
    def get(s: State): Int = value
  private case class Combo(getRegistry: State => Int) extends Operand:
    def get(s: State): Int = getRegistry(s)

  def literal(value: Int): Operand = value match
    case n if n >= 0 && n <= 7 => Literal(value)
    case _ =>  throw IllegalArgumentException("Literal operand must a be positive 8 bit number")
  def combo(value: Int): Operand = value match
    case n if n >= 0 && n <= 3 => literal(value)
    case 4 => Combo(_.x)
    case 5 => Combo(_.y)
    case 6 => Combo(_.z)
    case _ => throw IllegalArgumentException("Combo operand must a be positive 8 bit number except 7")
