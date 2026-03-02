import scala.math.Ordering.Implicits._

trait InputReader[T]:
  def read(p: Int): Either[Error, Option[T]]
class SequenceReader[T](val input:Seq[T]) extends InputReader[T]:
  override def read(p: Int): Either[Error, Option[T]] = p match
    case n if n < 0 => Left(Error(s"Attempt to read input out of bounds at $n"))
    case n => Right(if input.isDefinedAt(n) then Some(input(n)) else None)
  def mapValues[B](f: T => B): SequenceReader[B] = SequenceReader(input.map(f))
  def mapInput[B](f: Seq[T] => Seq[B]): SequenceReader[B] = SequenceReader[B](f(input))

object GuardedSequenceReader:
  private type Guard[T] = T => Either[Error, T]
  sealed case class GuardedBuilder[T](override val input:Seq[T], guards: Set[Guard[T]])
    extends SequenceReader[T](input):
    override def read(p: Int): Either[Error, Option[T]] = super.read(p) match
      case v@Left(value) => v
      case Right(value) => value match
        case Some(x) => guards.foldLeft(Right(x): Either[Error, T])(_.flatMap(_)).map(Some(_))
        case None => Right(None)


  extension[T](reader: SequenceReader[T])
    def withGuards: GuardedBuilder[T] = GuardedBuilder(reader.input, Set())
  extension [T](builder: GuardedBuilder[T])
    def + (guard: Guard[T]): GuardedBuilder[T] = builder.copy(guards = builder.guards + guard)

  def min[T:Ordering](min: T): Guard[T] = {
      case v if v < min => Left(Error(s"Values must be >= $min"))
      case v => Right(v)
    }

  def max[T:Ordering](max: T): Guard[T] = {
      case v if v > max => Left(Error(s"Values must be <= $max"))
      case v => Right(v)
    }













