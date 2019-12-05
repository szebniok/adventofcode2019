import scala.annotation.tailrec
import scala.io.Source
object day5 extends App {
  val initialOpcodes = Source
    .fromFile("day5input.txt")
    .getLines
    .next
    .split(",")
    .map(_.toInt)
    .toVector

  @tailrec
  def runProgram(opcodes: Vector[Int],
                 input: List[Int],
                 output: List[Int] = List(),
                 offset: Int = 0): List[Int] = {
    val registers = opcodes
    val code = opcodes.drop(offset)

    val firstInstruction = code.head
    val operationCode = firstInstruction % 100
    val firstArgumentMode = (firstInstruction / 100) % 10
    val secondArgumentMode = (firstInstruction / 1000) % 10
    val thirdArgumentMode = (firstInstruction / 10000) % 10

    code.drop(1).prepended(operationCode) match {
      case Vector(99, _*) => output

      case Vector(mathOperationCode, src1, src2, dest, _*)
          if (Seq(1, 2) contains mathOperationCode) =>
        val src1Value =
          getValueFromRegisters(registers, src1, firstArgumentMode)
        val src2Value =
          getValueFromRegisters(registers, src2, secondArgumentMode)
        val updatedRegisters = registers.updated(
          dest,
          getMathOperationFromCode(mathOperationCode)(src1Value, src2Value)
        )
        runProgram(updatedRegisters, input, output, offset + 4)

      case Vector(3, index, _*) =>
        runProgram(
          registers.updated(index, input.head),
          input.drop(1),
          output,
          offset + 2
        )

      case Vector(4, index, _*) =>
        runProgram(
          registers,
          input,
          output.appended(
            getValueFromRegisters(registers, index, firstArgumentMode)
          ),
          offset + 2
        )

      case Vector(booleanTest, test, address, _*)
          if (Seq(5, 6) contains booleanTest) =>
        val testValue =
          getValueFromRegisters(registers, test, firstArgumentMode)
        val addressValue =
          getValueFromRegisters(registers, address, secondArgumentMode)
        val updatedOffset =
          if (getBooleanTestFromCode(booleanTest)(testValue)) addressValue
          else offset + 3
        runProgram(registers, input, output, updatedOffset)

      case Vector(equalityTest, test1, test2, dest, _*)
          if (Seq(7, 8) contains equalityTest) =>
        val test1Value =
          getValueFromRegisters(registers, test1, firstArgumentMode)
        val test2Value =
          getValueFromRegisters(registers, test2, secondArgumentMode)
        val updatedRegisters = registers.updated(
          dest,
          if (getEqualityTestFromCode(equalityTest)(test1Value, test2Value)) 1
          else 0
        )
        runProgram(updatedRegisters, input, output, offset + 4)
    }
  }

  def getMathOperationFromCode(code: Int): (Int, Int) => Int =
    if (code == 1) _ + _
    else _ * _

  def getBooleanTestFromCode(code: Int): (Int) => Boolean =
    if (code == 5) _ != 0
    else _ == 0

  def getEqualityTestFromCode(code: Int): (Int, Int) => Boolean =
    if (code == 7) _ < _
    else _ == _

  def getValueFromRegisters(registers: Vector[Int],
                            index: Int,
                            mode: Int): Int =
    if (mode == 1) index
    else registers(index)

  println(runProgram(initialOpcodes, List(1)).find(_ > 0).head)

  // 2 star
  println(runProgram(initialOpcodes, List(5)).head)
}
