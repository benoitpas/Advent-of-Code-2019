object Day8 {
    import scala.io.Source
   
    val input = Source.fromResource("day8.txt").getLines.next
    val testInput = (3*2,"123456789012")

    def split(n:Int, s:String) = {
        val steps = (n to s.length by n).toList
        val r = steps.foldLeft(s,List[String]())((a,i) => {
            val (s1,s2) = a._1.splitAt(n)
            (s2,s1::a._2)
        })
        r._2
    }

    def main(args: Array[String]): Unit = {
        val layerLess0 = split(25*6, input).map( (s:String) => (s,s.count(_ == '0'))).sortBy(_._2).head._1
        val part1 = layerLess0.count(_ == '1') * layerLess0.count(_ == '2')
        println(s"part1 = $part1")


    }
}