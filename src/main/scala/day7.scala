import scala.io.Source

object day7 extends App {
  sealed trait Mode
  final object Immediate extends Mode
  final object Address extends Mode
  object Mode {
    def fromInt(value: Int): Mode = if (value == 1) Immediate else Address
  }

  case class Program(opcodes: Vector[Int], input: LazyList[Int], ip: Int = 0) {
    def readValue(source: Int, mode: Mode): Int = mode match {
      case Immediate => source
      case Address   => opcodes(source)
    }

    def setValue(destination: Int, value: Int): Program =
      copy(opcodes = opcodes.updated(destination, value))

    def moveIp(offset: Int): Program = copy(ip = ip + offset)

    def consumeInput(dest: Int): Program =
      copy(input = input.tail).setValue(dest, input.head)

    def output(): LazyList[Int] = LazyList.unfold(this)(runOneTick).flatten
  }

  def runOneTick(program: Program): Option[(Option[Int], Program)] = {
    val code = program.opcodes.drop(program.ip)

    val firstInstruction = code.head
    val operationCode = firstInstruction % 100
    val modes = List(100, 1000, 10000)
      .map(d => (firstInstruction / d) % 10)
      .map(Mode.fromInt)

    code.drop(1).prepended(operationCode) match {
      case Vector(99, _*) => None
      case Vector(mathOpCode, src1, src2, dest, _*)
          if (Seq(1, 2) contains mathOpCode) =>
        val newValue = getMathOpFromCode(mathOpCode)(
          program.readValue(src1, modes(0)),
          program.readValue(src2, modes(1))
        )
        Some(None, program.setValue(dest, newValue).moveIp(4))

      case Vector(3, dest, _*) =>
        Some(None, program.consumeInput(dest).moveIp(2))

      case Vector(4, src, _*) =>
        Some(Some(program.readValue(src, modes(0))), program.moveIp(2))

      case Vector(booleanTest, test, address, _*)
          if (Seq(5, 6) contains booleanTest) =>
        val jumpValue =
          if (getBooleanTestFromCode(booleanTest)(
                program.readValue(test, modes(0))
              )) program.readValue(address, modes(1)) - program.ip
          else 3
        Some(None, program.moveIp(jumpValue))

      case Vector(equalityTest, test1, test2, dest, _*)
          if (Seq(7, 8) contains equalityTest) =>
        val testValue =
          if (getEqualityTestFromCode(equalityTest)(
                program.readValue(test1, modes(0)),
                program.readValue(test2, modes(1))
              )) 1
          else 0
        Some(None, program.setValue(dest, testValue).moveIp(4))
    }
  }

  def getMathOpFromCode(code: Int): (Int, Int) => Int =
    if (code == 1) _ + _ else _ * _

  def getBooleanTestFromCode(code: Int): (Int) => Boolean =
    if (code == 5) _ != 0 else _ == 0

  def getEqualityTestFromCode(code: Int): (Int, Int) => Boolean =
    if (code == 7) _ < _ else _ == _

  val initialOpcodes: Vector[Int] =
    Source
      .fromFile("day7input.txt")
      .getLines()
      .next()
      .split(",")
      .map(_.toInt)
      .toVector

  def runPermutation(permutation: List[Int]): Int =
    permutation.foldLeft(0) {
      case (inputSignal, phase) =>
        Program(initialOpcodes, LazyList(phase, inputSignal)).output.last
    }

  println(List(0, 1, 2, 3, 4).permutations.map(runPermutation).max)

  // 2 star
  def runPermutationsLooped(permutation: List[Int]): Int = {
    def aProgram: Program =
      Program(initialOpcodes, permutation(0) #:: 0 #:: eProgram.output)
    def bProgram: Program =
      Program(initialOpcodes, permutation(1) #:: aProgram.output)
    def cProgram: Program =
      Program(initialOpcodes, permutation(2) #:: bProgram.output)
    def dProgram: Program =
      Program(initialOpcodes, permutation(3) #:: cProgram.output)
    def eProgram: Program =
      Program(initialOpcodes, permutation(4) #:: dProgram.output)

    eProgram.output.last
  }

  println(List(5, 6, 7, 8, 9).permutations.map(runPermutationsLooped).max)
}
