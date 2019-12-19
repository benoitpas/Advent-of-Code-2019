object Day9 {
    import scala.io.Source
   
    val input = Source.fromResource("day9.txt").getLines.next
    val program = Day5.toProgram(input)

    val testProgram1 = List[Long](109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
    val testProgram2 = List[Long](1102,34915192,34915192,7,4,7,99,0)
    val testProgram3 = List[Long](104,1125899906842624L,99)

    def main(args: Array[String]): Unit = {
        val tp1 = new Array[Long](200)
        testProgram1.copyToArray(tp1)
        val o1 = Day5.run(tp1,List())
        println(s"o1 = $o1")
        val o2 = Day5.run(testProgram2.toArray,List())
        println(s"o2 = $o2")
        val o3 = Day5.run(testProgram3.toArray,List())
        println(s"o3 = $o3")

        val p1 = new Array[Long](2000)
        program.copyToArray(p1)
        val part1 = Day5.run(p1,List(1L))
        println(s"part1 = $part1")
    }
}