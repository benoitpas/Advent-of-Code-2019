object Day2 {
    import scala.io.Source

    // Stored as an immutable Slist so that when I use it as an array. a copy is made
    def p1 = List(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,5,19,23,1,23,5,27,1,27,13,31,1,31,5,35,1,9,35,39,2,13,39,43,1,43,10,47,1,47,13,51,2,10,51,55,1,55,5,59,1,59,5,63,1,63,13,67,1,13,67,71,1,71,10,75,1,6,75,79,1,6,79,83,2,10,83,87,1,87,5,91,1,5,91,95,2,95,10,99,1,9,99,103,1,103,13,107,2,10,107,111,2,13,111,115,1,6,115,119,1,119,10,123,2,9,123,127,2,127,9,131,1,131,10,135,1,135,2,139,1,10,139,0,99,2,0,14,0)

    def main(args: Array[String]): Unit = {
        println(run1(Array(1,0,0,0,99)).toList)
        println(run1(Array(2,3,0,3,99)).toList)
        println(run1(Array(2,4,4,5,99,0)).toList)
        println(run1(Array(1,1,1,4,99,5,6,0,99)).toList)
        println(run1(Array(1,9,10,3,2,3,11,0,99,30,40,50)).toList)
        println("day 2, result 1 :" + run(p1.toArray, 12, 2).toList)
        // first iterate on verb, then on  noun
        val target = 19690720
        // First find verb
        val nounList = LazyList.from(12).map(i => (i,run(p1.toArray,i,1)(0))).takeWhile(_._2 <= target)
        println(nounList.toList)
        val noun = nounList.last._1
        println("noun="+noun)
        val verb = target - nounList.last._2 + 1
        println("verb="+verb)

        println(run(p1.toArray, noun, verb).toList)
        println("day 2, result 2 :" +  (noun * 100 + verb))
    }

    // runs the program and returns updated memory
    def run(p:Array[Int], noun:Int, verb:Int) = {
        p.update(1,noun)
        p.update(2,verb)
        run1(p)
    }

    def run1(p:Array[Int]) = {
        var i = 0
        while(p(i) != 99) {
            p(i) match {
                case 1 => p.update(p(i+3),p(p(i+1))+p(p(i+2))) ; i = i + 4
                case 2 => p.update(p(i+3),p(p(i+1))*p(p(i+2))) ; i = i + 4
                case _ => throw new Exception("Unknown opcode")
            }
        }
        p

    }
}