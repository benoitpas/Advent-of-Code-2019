
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

    def run1(program:List[Long], phases:List[Long]) = phases.foldLeft(List[Long](0))((input,phase) => {
        val o = Day5.run(program.toArray, phase::input)._1.toList
        //println(s"${phase::input} $o")
        o
    })

    val pTest21 = List(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
    val phases21 = List(9,8,7,6,5)

    val pTest22 = List(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
        -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
        53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)
    val phases22 = List(9,7,8,5,6)

    def run2(program:List[Long],phases:List[Long]) = {
        val n = phases.length
        val amps = List.fill(n)(program.toArray)

        // Initialize the amps with the phase
        val iOutputs = (phases zip amps).map((p,a) => Day5.run(a,List(p),Some((0,0))))
        val iIndexesBases = iOutputs map (o => o._2)
        //println("iOutputs = $iOutputs")

        // ib:Indexes and Bases
        def loop(signal:Long, ib:List[(Int,Int)]):Long = {
            // Then run them
            val outputs = (ib zip amps).foldLeft((signal,List[(Int,Int)]()))((acc,iAmp) => {
                val o = Day5.run(iAmp._2,List(acc._1),Some(iAmp._1))
                val s = o._1.headOption.getOrElse(signal)
                //println(s"s=$s o=$o")
                (s, acc._2 ++ List(o._2))
            })
            //println(outputs)
            if (amps(0)(outputs._2.head._1) != 99) {
                loop(outputs._1, outputs._2)
            } else {
                outputs._1
            }


        }

        loop(0, iIndexesBases)
    }

    def main(args: Array[String]): Unit = {

        /*
        val output1 = run(pTest1, phases1)
        println(s"1 $output1")
        val output2 = run(pTest2, phases2)
        println(s"2 $output2")
        val output3 = run(pTest3, phases3)
        println(s"3 $output3")
        */
        val part1 = List[Long](0,1,2,3,4).permutations.map(run1(program,_).head).toList.sorted.last
        println(s"part1 = $part1")

        //println(run2(pTest21, phases21))
        //println(run2(pTest22, phases22))

        val part2 = List[Long](9,8,7,6,5).permutations.map(run2(program,_)).toList.sorted.last
        println(s"part2 = $part2")


    }
}