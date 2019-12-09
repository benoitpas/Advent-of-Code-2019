object Day5 {
    import scala.io.Source
    val program = Source.fromResource("day5.txt").getLines.next.split(",").map(Integer.parseInt(_).toInt).toList

    def main(args: Array[String]): Unit = {
        println(program)
    }

    def run(p:Array[Int], input:List[Int]) = {
        var i = 0
        var output = List[Int]()
        while(p(i) != 99) {
            p(i) match {
                case 1 => p.update(p(i+3),p(p(i+1))+p(p(i+2))) ; i = i + 4
                case 2 => p.update(p(i+3),p(p(i+1))*p(p(i+2))) ; i = i + 4
                case _ => throw new Exception("Unknown opcode")
            }
        }

    }
}