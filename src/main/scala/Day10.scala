object Day10 {
    import scala.io.Source
   
    val input = Source.fromResource("day10.txt").getLines

    type Point = (Int,Int)
    def getPoints(strings:List[String]) : List[Point] = {
        val height = strings.length
        val width = strings.head.length

        val r = for {
            (y,s) <- (0 to height-1) zip strings
            (x,c) <- (0 to width-1) zip s
            if (c == '#')
        } yield (x,y)
        r.toList
    }

    def getLine(p1:Point,p2:Point) = {
        val dx = math.abs(p2._1 - p1._1)
        val dy = math.abs(p2._2 - p1._2)
        if (dx<dy) {
            getLineX((p1._2,p1._1),(p2._2,p2._1)).map((y,x) => (x,y))
        } else getLineX(p1,p2)
    }


    def getLineX(p1:Point,p2:Point) = {
        val dx = math.abs(p2._1 - p1._1)
        val dyDiff = p2._2 - p1._2
        val (dy,yStep) = if (dyDiff > 0) (dyDiff,1) else (-dyDiff,-1)
        val delta = 2*dy - dx
        val xRange = if (p1._1 <= p2._1)  (p1._1 to p2._1) else (p1._1 to p2._1 by -1)
        val r = xRange.foldLeft((List[Point](),p1._2,delta))((a,x) => {
            val y = a._2
            val delta = a._3
            val (nY,nDelta) = if (delta > 0 ) (y + yStep,delta - 2*dx + 2*dy) else (y,delta + 2*dy)
            ((x,y)::a._1,nY,nDelta)
        })

        r._1
    }

    def main(args: Array[String]): Unit = {
    }
}
