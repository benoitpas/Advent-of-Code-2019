object Day5 {
    import scala.io.Source
    val program = Source.fromResource("day5.txt").getLines.next.split(",").map(Integer.parseInt(_).toInt).toList
    val testInputOutput = List(3,0,4,0,99)
    val testDirect = List(1101,3,4,0,4,0,99)
    val testMixed = List(101,0,4,0,4,0,99)

    def main(args: Array[String]): Unit = {
        val input = List(1)
        val output = run(program.toArray, input)
        println("output="+output)
    }


    def run(p:Array[Int], input:List[Int]) = {

        var i = 0
        var inputIndex = 0
        import scala.collection.mutable.ListBuffer
        var output = ListBuffer[Int]()
        while(p(i) % 100 != 99) {
            val opcode = p(i)
            print(s"$i\t$opcode\t")

            def getParam(index:Int) = {
                val mask = Map(1 -> 100, 2 -> 1000)
                if ((opcode / mask(index)) % 10 > 0) 
                    p(i+index) 
                else
                    p(p(i+index))
            }

            def unaryOp(f : (Int) => Unit) = {
                val p1 = getParam(1)
                print(s"${p(i+1)}\t($p1)")
                f(p1)
                2
            }

            def binaryOp(f : (Int,Int) => Int) = {
                val p1 = getParam(1)
                val p2 = getParam(2)
                print(s"${p(i+1)}\t${p(i+2)}\t${p(i+3)}\t($p1,$p2)")
                p.update(p(i+3), f(p1, p2))
                4
            }

            val inc = opcode % 100 match {
                case 1 => binaryOp(_ + _)
                case 2 => binaryOp(_ * _)
                case 3 => print(input(inputIndex)) ; p.update(p(i+1), input(inputIndex)) ; inputIndex = inputIndex + 1 ; 2
                case 4 => unaryOp( output += _ )
                case o => throw new Exception("Unknown opcode "+o+" position "+i)
            }
            i = i + inc
            println
        }
        output

    }
}