object Day5 {
    import scala.io.Source
    val input = Source.fromResource("day5.txt").getLines.next
    def toProgram(s:String) = s.split(",").map(_.toLong).toList
    val program = toProgram(input)

    val testInputOutput = List(3,0,4,0,99)
    val testDirect = List(1101,3,4,0,4,0,99)
    val testMixed = List(101,0,4,0,4,0,99)
    val testEqual8 = List(3,9,8,9,10,9,4,9,99,-1,8)
    val testLessThan8 = List(3,9,7,9,10,9,4,9,99,-1,8)
    val testEqual8i = List(3,3,1108,-1,8,3,4,3,99)
    val testLessThan8i = List(3,3,1107,-1,8,3,4,3,99)
    val testJumpPosition = List(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    val testJumpImmediate = List(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
    val compareTo8 = List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)

    def main(args: Array[String]): Unit = {
        val input = List(5L)
        val output = run(program.toArray, input)
        println("output="+output)
    }

    import scala.collection.mutable.ListBuffer
    def run(p:Array[Long], input:List[Long], stream:Option[(Int,Int)] = None, verbose:Boolean = false) : (ListBuffer[Long], (Int,Int)) = {

        def vprint(s:String) = if (verbose) print(s)

        var (i,base) = stream.getOrElse((0,0))
        var inputIndex = 0
        var stopInput = false
        var output = ListBuffer[Long]()
        while(p(i) % 100 != 99 && !(stream.isDefined && output.length>0) && !stopInput) {
            val opcode = p(i)
            vprint(s"$i\t$opcode\t")

            def getMode(index:Int) = {
                val mask = Map(1 -> 100, 2 -> 1000, 3 -> 10000)
                (opcode / mask(index)) % 10
            }

            def getParam(index:Int) = {
                getMode(index) match {
                    case 0 => p(p(i + index).toInt)
                    case 1 => p(i + index) 
                    case 2 => p(p(i + index).toInt + base)
                }
            }

            def update(index:Int, v:Long) = {
                val offset = 
                    getMode(index) match {
                        case 2 => base
                        case 1 => throw new Exception(s"Immediate mode no supported for output (opcode $opcode position $i")
                        case 0 => 0
                    }
                p.update(p(i+index).toInt + offset, v)
            }

            def readInput() = 
                if (inputIndex<input.length) {
                    vprint(input(inputIndex).toString)
                    update(1, input(inputIndex))
                    inputIndex = inputIndex + 1
                    2
                } else {
                    stopInput = true
                    0
                }

            def unaryOp(f : (Long) => Unit) = {
                val p1 = getParam(1)
                vprint(s"${p(i+1)}\t($p1)")
                f(p1)
                2
            }

            def binaryOp(f : (Long,Long) => Long) = {
                val p1 = getParam(1)
                val p2 = getParam(2)
                vprint(s"${p(i+1)}\t${p(i+2)}\t${p(i+3)}\t($p1,$p2)")
                update(3, f(p1, p2))
                4
            }

            def jumpIf(c:Boolean) = {
                val p1 = getParam(1)
                val p2 = getParam(2).toInt
                val jump = (p1 != 0 && c) || (p1 == 0 && !c)
                vprint(s"${p(i+1)}\t${p(i+2)}\t($jump,$p2)")
                if (jump)
                    p2 - i // jump to p1
                else 
                    3
            }

            def compare(f:(Long,Long) => Boolean) = {
                val p1 = getParam(1)
                val p2 = getParam(2)
                vprint(s"${p(i+1)}\t${p(i+2)}\t${p(i+3)}\t($p1,$p2)")
                update(3, if (f(p1, p2)) 1 else 0)
                4
            }

            val inc : Int = opcode % 100 match {
                case 1 => binaryOp(_ + _)
                case 2 => binaryOp(_ * _)
                case 3 => readInput()
                case 4 => unaryOp( output += _ )
                case 5 => jumpIf(true)
                case 6 => jumpIf(false)
                case 7 => compare(_ < _)
                case 8 => compare(_ == _)
                case 9 => unaryOp( (v) => base = base + v.toInt )
                case o => throw new Exception("Unknown opcode "+o+" position "+i)
            }
            i = i + inc.toInt
            if (verbose) println
        }
        (output,(i,base))

    }
}