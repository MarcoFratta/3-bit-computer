import scala.util.{Failure, Success}

object SingleOperandComputer:
  
  // we can easily use the option/either flatmap to continue execution until
  // an option None is returned by any operation ( we also map Either->Left to None)
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

  // the idea is to check if from the current ip position we already
  // "landed" in a certain state s before. If true, then it will cause a loop. 
  // might not work with any instruction set! 
  def withLoopDetection[S: IpOps, I, O](instructions: Map[I, Instruction[S, I, O]])(initState: S):
  Computer[I, Seq[String]] =
    var hist = Map[Int, Set[S]]()
    step(initState)(instructions)({
      case (s, o) if hist.isDefinedAt(s.ip) && hist(s.ip).contains(s)
      => (Some(s"Loop detected at ${s.ip}"), None)
      case (s, o) =>
        hist = hist + (s.ip -> (hist.getOrElse(s.ip, Set()) + s))
        o.map(_.toString) -> Some(s)
    })

  // we keep creating until the internal state of unfold is not None. 
  // any failure or ending the input will set the state to None, halting the unfold.
  private def step[S: IpOps, I, O](initState: S)(instructions: Map[I, Instruction[S, I, O]])
                                  (f: (S, Option[O]) => (Option[String], Option[S])): Computer[I, Seq[String]] = i =>
    LazyList.unfold(Option(initState))(s => for
      // reading an emtpy state will stop execution here
      state <- s
      // first operand
      r1 = i.read(state.ip)
      // second operand
      r2 = i.read(state.ip + 1)
    yield (r1, r2) match
      case (Left(error), _) => (Option(error.getMessage), None)
      case (_, Left(error)) => (Option(error.getMessage), None)
      case (Right(Some(_)), Right(None)) => (Some(s"Unexpected end of input, expected operand at ${state.ip + 1}"), None)

      case (Right(Some(opCode)), Right(Some(operand))) => instructions.get(opCode).map(_(state, operand) match
          case Failure(exception) => (Some(exception.getMessage), None)
          case Success(v) => f(v._1, v._2)
        )
        .getOrElse((Some(s"Instruction not found with OpCode $opCode"), None))
      // gracefully stop execution
      case _ => (None, None)).flatten

  def withErrors[S, I, O](instructions: Map[I, Instruction[S, I, O]])(initState: S)
                         (using ip: IpOps[S]): Computer[I, Seq[String]] =
    step(initState)(instructions)((s, o) => (o.map(_.toString), Some(s)))