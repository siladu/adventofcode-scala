import scala.io.Source

object Day9 {

  val input0 = "<>" //0
  val input1 = "<random characters>" //17
  val input2 = "<<<<>" //3
  val input3 = "<{!>}>" //2
  val input4 = "<!!>" //0
  val input5 = "<!!!>>" //0
  val input6 = "<{o\"i!a,<{i<a>" //10

  val input: Seq[Char] = Source.fromFile("day9input.txt").toList

  var totalGarbageCount = 0
  var currentGarbage = 0

  def increment: Unit = {
    totalGarbageCount = totalGarbageCount + 1
    currentGarbage = currentGarbage + 1
  }

  def processGarbage(chars: Seq[Char], prev: Char, start: Boolean = false): Seq[Char] = {
    print(if (chars.isEmpty) ' ' else chars.head)

    if (chars.isEmpty) Nil
    else if (chars.head.equals('<') && !prev.equals('!')) { if (!start) increment; processGarbage(chars.tail, chars.head) }
    else if (chars.head.equals('!') && prev.equals('!')) processGarbage(chars.tail, '-')
    else if (chars.head.equals('>') && !prev.equals('!')) { println(s"\ncurrentGarbage count = $currentGarbage, totalGarbageCount = $totalGarbageCount"); chars.tail }
    else { if (!chars.head.equals('!') && !prev.equals('!')) increment; processGarbage(chars.tail, chars.head) }
  }

  def processGroups(chars: Seq[Char], groupLevel: Int, groupLevels: List[Int]): List[Int] = {
    if (chars.isEmpty) groupLevels
    else if (chars.head.equals('{')) processGroups(chars.tail, groupLevel + 1, (groupLevel + 1) :: groupLevels)
    else if (chars.head.equals('}')) processGroups(chars.tail, groupLevel - 1, groupLevels)
    else {
      val rest = if (chars.head.equals('<')) { currentGarbage = 0; processGarbage(chars, ' ', true) } else chars.tail
      processGroups(rest, groupLevel, groupLevels)
    }
  }

//  runTestInput

  val result = processGroups(input, 0, Nil)
  println
  println(s"garbage count = $totalGarbageCount")

  def main(args: Array[String]): Unit = {
    println(result.sum)
  }

  def runTestInput = {
    println("TEST INPUT")
    totalGarbageCount = 0
    processGarbage(input0.toSeq, ' ', true)
    println(totalGarbageCount)
    totalGarbageCount = 0
    processGarbage(input1.toSeq, ' ', true)
    println(totalGarbageCount)
    totalGarbageCount = 0
    processGarbage(input2.toSeq, ' ', true)
    println(totalGarbageCount)
    totalGarbageCount = 0
    processGarbage(input3.toSeq, ' ', true)
    println(totalGarbageCount)
    totalGarbageCount = 0
    processGarbage(input4.toSeq, ' ', true)
    println(totalGarbageCount)
    totalGarbageCount = 0
    processGarbage(input5.toSeq, ' ', true)
    println(totalGarbageCount)
    totalGarbageCount = 0
    processGarbage(input6.toSeq, ' ', true)
    println(totalGarbageCount)
    totalGarbageCount = 0
    println("END TEST INPUT")
  }
}