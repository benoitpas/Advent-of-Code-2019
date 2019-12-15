
object Day7 {
    import scala.io.Source
   
    val input = Source.fromResource("day7.txt").getLines.next
    val program = Day5.toProgram(input)


    val pTest1 = List(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
    val phases1 = List(4,3,2,1,0)

    val pTest2 = List(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)
    val phases2 = List(0,1,2,3,4)

    val pTest3 = List(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
    val phases3 = List(1,0,4,3,2)

    def run1(program:List[Int], phases:List[Int]) = phases.foldLeft(List(0))((input,phase) => {
        val o = Day5.run(program.toArray, phase::input).toList
        //println(s"${phase::input} $o")
        o
    })

    def main(args: Array[String]): Unit = {

        /*
        val output1 = run(pTest1, phases1)
        println(s"1 $output1")
        val output2 = run(pTest2, phases2)
        println(s"2 $output2")
        val output3 = run(pTest3, phases3)
        println(s"3 $output3")
        */
        val part1 = List(0,1,2,3,4).permutations.map(run1(program,_).head).toList.sorted.last
        println(s"part1 = $part1")




    }
}