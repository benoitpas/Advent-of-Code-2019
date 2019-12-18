object Day9 {
    import scala.io.Source
   
    val input = Source.fromResource("day9.txt").getLines.next

    val testProgram1 = List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
    def main(args: Array[String]): Unit = {
        val tp1 = new Array[Int](200)
        testProgram1.copyToArray(tp1)
        val o1 = Day5.run(tp1,List(),verbose=true)
        println(s"o1 = $o1")
    }
}