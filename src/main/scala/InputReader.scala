trait InputReader[T]:
  def read(p: Int): Either[Error, Option[T]]
object InputReader:
  def apply[T](input: Seq[T]): InputReader[T] = {
    case n if n < 0 => Left(Error(s"Attempt to read input out of bounds at $n"))
    case n => Right(if input.isDefinedAt(n) then Some(input(n)) else None)
  }
object IntInputReader:
  extension(s:String)
    def toIntValues: Seq[Int] = s.split(',').map(_.toInt)
  
  class Builder(input:Seq[Int]):
    private var guards: Set[Int => Either[Error, Int]] = Set()
    
    def min(min:Int): Builder = 
      guards = guards + {
        case v if v < min => Left(Error(s"Values must be >= ${min}"))
        case v => Right(v)
      }
      this
    def max(max: Int): Builder = 
      guards = guards + {
        case v if v > max => Left(Error(s"Values must be <= $max"))
        case v => Right(v)
      }
      this
    def build(): InputReader[Int] = p => InputReader(input).read(p) match 
      case v @ Left(value) => v
      case Right(value) => value match 
        case Some(x) =>  guards.foldLeft(Right(x): Either[Error, Int])(_.flatMap(_)).map(Some(_))
        case None => Right(None)
      
       
    
      
    
      
  






