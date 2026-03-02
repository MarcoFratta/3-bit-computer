import Operands.OperandPolicy
import ThreeBitsState.Registry.*
import RegistryOps.*
import ThreeBitsState.{Registry, ValueType}

import scala.math.pow
import scala.util.Try
import scala.util.chaining.scalaUtilChainingOps

// we model an instruction as an operation that
// given a state and an input, produces an output
trait Instruction[S, I, O] extends ((S, I) => Try[(S, Option[O])])



object ThreeBitsInstruction:
  // IP incrementation (by default) depends on the number of operands needed by the instruction
  private val IP_STEP_NUMBER = 2
  private type I = ValueType
  private type O = ValueType
  private type Policy[S] = I => Operand[S, I]
  
  extension [S:IpOps](s:S)
    private def incrementIp: S = s.writeIp(s.ip + IP_STEP_NUMBER) 

  private def run[S](f: (S, I) => (S, Option[O])): Instruction[S, I, O] =
    (s, i) => Try { f(s, i) }
    
  def xdv[S:IpOps](operand: Policy[S])(using ops: RegistryOps[S, Registry, I]): Instruction[S, I, O] = run((s:S, i) =>
    ops.write(s) (
      X -> (s(X) / pow(2, operand(i)(s).doubleValue).toInt).intValue
    ).incrementIp 
      -> None)
  

  def yxl[S:IpOps](operand: Policy[S])(using ops: RegistryOps[S, Registry, I]): Instruction[S, I, O] =
    run((s, i) => ops.write(s)(
      Y -> (s(Y) ^ operand(i)(s))
    ).incrementIp  -> None)

  def yst[S:IpOps](bits: Int, getter: Policy[S])(using ops: RegistryOps[S, Registry, I]): Instruction[S, I, O] =
    run((s, i) => ops.write(s)( 
      Y -> getter(i)(s) % pow(2, bits).intValue,
    ).incrementIp -> None)

  def jnz[S:IpOps](getter: Policy[S]) (using ops: RegistryOps[S, Registry, I]): Instruction[S, I, O] = 
    run((s, i) =>  s(X) match
        case 0 => incrementIp(s) -> None
        case _ => s.writeIp(getter(i)(s).intValue) -> None
    )

  def yxz[S:IpOps](using ops: RegistryOps[S, Registry, I]): Instruction[S, I, O] =
    run((s, i) =>  ops.write(s)(
      Y -> (s(Y) ^ s(Z))
    ).incrementIp -> None)

  def out[S](bits: Int, getter: Policy[S])(using ops: RegistryOps[S, Registry, I], ip:IpOps[S]): Instruction[S, I, O] =
    run((s, i) => s.incrementIp ->  Some(getter(i)(s) % pow(2, bits).intValue))

  def ydv[S](getter: Policy[S]) (using ops: RegistryOps[S, Registry, I], ip:IpOps[S]): Instruction[S, I, O] =
    run((s, i) => xdv(getter)(s, i).map((s2, _) => ops.write(s)(Y -> s2(X)).incrementIp -> None).get)

  def zdv[S](getter: Policy[S]) (using ops: RegistryOps[S, Registry, I], ip:IpOps[S]): Instruction[S, I, O] =
    run((s, i) => xdv(getter)(s, i).map((s2, _) => ops.write(s)(Z -> s2(X)).incrementIp -> None).get)