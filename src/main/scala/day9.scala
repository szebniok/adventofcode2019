import scala.io.Source

object day9 extends App {
  sealed trait Mode
  final object Immediate extends Mode
  final object Address extends Mode
  final object Relative extends Mode
  object Mode {
    def fromLong(value: Long): Mode = value match {
      case 0 => Address
      case 1 => Immediate
      case _ => Relative
    }
  }

  case class Program(opcodes: Vector[Long],
                     input: LazyList[Long],
                     ip: Int = 0,
                     relativeBase: Int = 0) {
    def readValue(source: Long, mode: Mode): Long = mode match {
      case Immediate => source
      case Address   => opcodes(source.toInt)
      case Relative  => opcodes(source.toInt + relativeBase)
    }

    def setValue(destination: Long,
                 destinationMode: Mode,
                 value: Long): Program = {
      val offset = if (destinationMode == Relative) relativeBase else 0
      copy(opcodes = opcodes.updated(destination.toInt + offset, value))
    }

    def moveIp(offset: Int): Program = copy(ip = ip + offset)

    def consumeInput(dest: Int, mode: Mode): Program =
      copy(input = input.tail).setValue(dest, mode, input.head)

    def moveRelativeBase(offset: Int): Program =
      copy(relativeBase = relativeBase + offset)

    def output(): LazyList[Long] = LazyList.unfold(this)(runOneTick).flatten
  }

  def runOneTick(program: Program): Option[(Option[Long], Program)] = {
    val code = program.opcodes.drop(program.ip)

    val firstInstruction = code.head
    val operationCode = firstInstruction % 100
    val modes = List(100, 1000, 10000)
      .map(d => (firstInstruction / d) % 10)
      .map(Mode.fromLong)

    code.drop(1).prepended(operationCode) match {
      case Vector(99, _*) => None
      case Vector(mathOpCode, src1, src2, dest, _*)
          if (Seq(1, 2) contains mathOpCode) =>
        val newValue = getMathOpFromCode(mathOpCode)(
          program.readValue(src1, modes(0)),
          program.readValue(src2, modes(1))
        )
        Some(None, program.setValue(dest, modes(2), newValue).moveIp(4))

      case Vector(3, dest, _*) =>
        Some(None, program.consumeInput(dest.toInt, modes(0)).moveIp(2))

      case Vector(4, src, _*) =>
        Some((Some(program.readValue(src, modes(0))), program.moveIp(2)))

      case Vector(booleanTest, test, address, _*)
          if (Seq(5, 6) contains booleanTest) =>
        val jumpValue =
          if (getBooleanTestFromCode(booleanTest)(
                program.readValue(test, modes(0))
              )) program.readValue(address, modes(1)) - program.ip
          else 3
        Some(None, program.moveIp(jumpValue.toInt))

      case Vector(equalityTest, test1, test2, dest, _*)
          if (Seq(7, 8) contains equalityTest) =>
        val testValue =
          if (getEqualityTestFromCode(equalityTest)(
                program.readValue(test1, modes(0)),
                program.readValue(test2, modes(1))
              )) 1
          else 0
        Some(None, program.setValue(dest, modes(2), testValue).moveIp(4))

      case Vector(9, offset, _*) =>
        Some(
          None,
          program
            .moveRelativeBase(program.readValue(offset, modes(0)).toInt)
            .moveIp(2)
        )
    }
  }

  def getMathOpFromCode(code: Long): (Long, Long) => Long =
    if (code == 1) _ + _ else _ * _

  def getBooleanTestFromCode(code: Long): (Long) => Boolean =
    if (code == 5) _ != 0 else _ == 0

  def getEqualityTestFromCode(code: Long): (Long, Long) => Boolean =
    if (code == 7) _ < _ else _ == _

  val initialOpcodes: Vector[Long] =
    Source
      .fromFile("day9input.txt")
      .getLines()
      .next()
      .split(",")
      .map(_.toLong)
      .toVector

  println(
    Program(initialOpcodes.appendedAll(Vector.fill(10000)(0)), LazyList(1)).output.toList.head
  )

  // 2 star
  println(
    Program(initialOpcodes.appendedAll(Vector.fill(10000)(0)), LazyList(2)).output.toList.head
  )
}
