package computer

import scala.util.Try
// given an input I from an input Reader, produces an output O
trait Computer[I, O]:
  def compute(input: InputReader[I]): O
// type class to add a registry to a computer state
trait RegistryOps[S, K, V]:
  extension (state: S)
    def read(k: K): Option[V]
    def write(updates: (K, V)*): S
// type class to add an istruction pointer to a state
trait IpOps[S]:
  extension (state: S)
    def ip: Int
    def writeIp(v: Int): S
// models different ways to map the input to the state
// for example a combo operand 4, should select the X registry from the state
trait Operand[S, R] extends (S => R)

// we model an instruction as an operation that
// given a state S and an input I, produces an output O (we use Try since it can fail)
trait Instruction[S, I, O] extends ((S, I) => Try[(S, Option[O])])

// a component that knows how to read the input at poisiton p
// useful to abstract the computer type from the input 
trait InputReader[T]:
  def read(p: Int): Either[Error, Option[T]]

object RegistryOps:
  extension [S](s: S)
    def apply[K, V](k: K)(using ops: RegistryOps[S, K, V]): V = s.read(k).get