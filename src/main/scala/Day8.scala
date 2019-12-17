object Day8 {
    import scala.io.Source
   
    val input = Source.fromResource("day8.txt").getLines.next
    val testInput = (3*2,"123456789012")
    val testInput2 = "0222112222120000"

    def split(n:Int, s:String) : List[String] = {
        val steps = (n to s.length by n).toList
        val r = steps.foldLeft(s,List[String]())((a,i) => {
            val (s1,s2) = a._1.splitAt(n)
            (s2,a._2 :+ s1)
        })
        r._2
    }

    def main(args: Array[String]): Unit = {
        val width = 25
        val height = 6
        val layers = split(width*height, input)
        val layerLess0 = layers.map( (s:String) => (s,s.count(_ == '0'))).sortBy(_._2).head._1
        val part1 = layerLess0.count(_ == '1') * layerLess0.count(_ == '2')
        println(s"part1 = $part1")

        val layer : String = layers.reduce 
            { (l1:String,l2:String) =>  
                val r = (l1  zip l2).map {
                    case ('2',v) => v
                    case (v,_) => v
                }
                r.mkString 
            }

        println("Part 2:")
        split(width,layer).map((s) => println(s.replace('0',' ')))
    }
}