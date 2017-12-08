import scala.io.Source

object DaySeven {

  val testInput = """pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
"""
  case class Program(name: String, weight: Int, children: List[String])

    val input = Source.fromFile("day7input.txt").getLines()
//  val input = testInput.lines
  val programs: Seq[Program] = input.map(parseLine).toSeq


  def parseLine(line: String): Program = {
    val values: List[String] = line.split(Array(' ', '(')).toList
      .filterNot(_.isEmpty)
      .filter(s => s.matches("^[a-z|\\d].*"))
      .map(s => s.replace(",", ""))
      .map(s => s.replace(")", ""))

    values match {
      case name :: weight :: children => new Program(name, weight.toInt, children)
      case _ => new Program("", -1, Nil)
    }
  }

  def findRoot(programs: Seq[Program]): Program = {
    // find element that doesn't appear in any other children
    val root: Seq[Program] = programs.filterNot(p => programs.exists(_.children.contains(p.name)))
    // OR is this cleaner?
    // program names diffed with list of all children
    val allChildren = programs flatMap { _.children }
    val root2 = programs map { _.name } diff allChildren

    root.head
  }

  val partOne = {
    findRoot(programs).name
  }

  val partTwo = {
    def findProgram(name: String): Program = programs.find(_.name == name).get

    // find children weights at each level
    def calculateWeight(program: Program): Int = {
      if (program.children.isEmpty) program.weight
      else {
        val childPrograms: Seq[Program] = program.children map findProgram
        val weightsForLevel: Seq[Int] = childPrograms map calculateWeight
        println(s"$program: = $weightsForLevel + ${program.weight} = ${weightsForLevel.sum + program.weight}")
        weightsForLevel.sum + program.weight
      }
    }

    calculateWeight(findRoot(programs))
  }

  def main(args: Array[String]): Unit = {
    println(partOne)
    println(partTwo)
  }
}