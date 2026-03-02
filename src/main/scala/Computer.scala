import BaseState.IP

import scala.util.{Failure, Success}
import RegistryOps.*


trait Computer[I, O]:
  def compute(input: InputReader[I]): O

enum BaseState:
  case IP

object SingleOperandComputer:

  private def withErrorLogging[T](v: Either[Error, Option[T]]): Option[String] = v match
    case Left(value) => Some(value.getMessage)
    case Right(value) => Some(value.toString)

  def ignoreErrors[S, I, O](instructions: Map[I, Instruction[S, I, O]])(initState: S)
                           (using ip: IpOps[S]): Computer[I, Seq[O]] = i =>
    LazyList.unfold(initState)(s => for
      // read the opcode based on ip
      opCode <- i.read(s.ip).toOption.flatten
      // get the corresponding instruction
      instruction <- instructions.get(opCode)
      // read the second operand
      operand <- i.read(s.ip + 1).toOption.flatten
      // apply instruction with the given operand
      (s2, out) <- instruction(s, operand).toOption
    yield (out, s2)).flatten


  def withLoopDetection[S, I, O](instructions: Map[I, Instruction[S, I, O]])(initState: S)
                                (using ip: IpOps[S]): Computer[I, Seq[String]] = i =>
    LazyList.unfold(Option(initState, Map[Int, S]()))(s => for
      // reading na emtpy state will stop execution
      (state, hist) <- s
      r1 = i.read(state.ip)
      r2 = i.read(state.ip + 1)
    yield (r1, r2) match
      case (Left(error), _) => (Option(error.getMessage), None)
      case (_, Left(error)) => (Option(error.getMessage), None)
      case (Right(Some(_)), Right(None)) => (Some(s"Unexpected end of input, expected operand at ${state.ip+1}"), None)
      case (Right(Some(opCode)), Right(Some(operand))) => instructions.get(opCode).map(i =>
        i(state, operand) match
          case Failure(exception) => (Some(exception.getMessage), None)
          case Success(v) if hist.isDefinedAt(state.ip) && hist(state.ip).equals(v._1) =>
            (Some(s"Loop detected at ${state.ip}"), None)
          case Success(v) => (v._2.map(_.toString), Some((v._1, hist + (state.ip -> v._1))))
      ).getOrElse((Some(s"Instruction not found with OpCode $opCode"), None))
      // gracefully stop execution
      case _ => (None, None)).flatten


  def withErrors[S, I, O](instructions: Map[I, Instruction[S, I, O]])(initState: S)
                         (using ip: IpOps[S]): Computer[I, Seq[String]] = i =>
    LazyList.unfold(Option(initState))(s => for
      // reading na emtpy state will stop execution
      state <- s
      r1 = i.read(state.ip)
      r2 = i.read(state.ip + 1)
    yield (r1, r2) match
      case (Left(error), _) => (Option(error.getMessage), None)
      case (_, Left(error)) => (Option(error.getMessage), None)
      case (Right(Some(_)), Right(None)) => (Some(s"Unexpected end of input, expected operand at ${state.ip+1}"), None)
      case (Right(Some(opCode)), Right(Some(operand))) => instructions.get(opCode).map(i =>
        i(state, operand) match {
          case Failure(exception) => (Some(exception.getMessage), None)
          case Success(v) =>  (v._2.map(_.toString), Some(v._1))
        }
      ).getOrElse((Some(s"Instruction not found with OpCode $opCode"), None))
      // gracefully stop execution
      case _ => (None, None)).flatten