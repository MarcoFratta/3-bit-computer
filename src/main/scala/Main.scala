import computer.GuardedSequenceReader.{max, min, withGuards}
import computer.SequenceReader
import threeBits.ThreeBitsComputer
import threeBits.ThreeBitsState.state


object Main:
  @main def main3BitComputer(x: Int, y:Int, z:Int, program:String*):Unit =
    val input = SequenceReader(program.mkString.trim.split(','))
      .mapValues(_.toInt)
      .withGuards + min(0) + max(7)
    val computer = ThreeBitsComputer.withLoopDetection
    val initState = state(0, x, y, z)
    println(s"result: ${computer(initState).compute(input).mkString(",")}")

