object Day3 {
    import scala.io.Source
   
    val i3 = Source.fromResource("day3.txt").getLines
    val p1 = i3.next
    val p2 = i3.next
    val s1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    //val p1 = "R8,U5,L5,D3"
    //val p2 = "U7,R6,D4,L4"
    //val p1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    //val p2 = "U62,R66,U55,R34,D71,R55,D58,R83"
    //val p1 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    //val p2 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"


    def main(args: Array[String]): Unit = {

        val p1p = parse(p1)
        val p2p = parse(p2)
        println(p1p)
//        println("1=" + generatePoints(p1p))
        println(p2p)
//        println("2=" + generatePoints(p2p))
        println("c=" + closestPoints(p1p,p2p))
        println("s=" + getSignalDelay(p1p,p2p))

        //println(closestPoints(i3.next,i3.next))
    }

    type Cmd =  (Char,Int)
    type Cmds =  List[(Char,Int)]
    def parse(s:String) : Cmds = s.split(",").map( e => (e.charAt(0),Integer.parseInt(e.drop(1)))).toList

    def generatePoints(c:Cmds) : Seq[(Int,Int)] = c.foldLeft(List((0,0))) { (a,c) => a ++ generatePoints(a.last._1, a.last._2, c) }.tail


    def generatePoints(x:Int, y:Int, c:Cmd) : Seq[(Int,Int)] = c._1 match {
        case 'R' => xPoints(x:Int, y:Int, c._2)
        case 'L' => xPoints(x:Int, y:Int, -c._2)
        case 'U' => yPoints(x:Int, y:Int, c._2)
        case 'D' => yPoints(x:Int, y:Int, -c._2)
    }

    import scala.language.implicitConversions
    def getRange(nb:Int) = if ( 0 <= nb) (1 to nb) else (-1 to nb by -1)
    def xPoints(x:Int, y:Int, nb:Int) = getRange(nb).map( v => (x+v,y))
    def yPoints(x:Int, y:Int, nb:Int) = getRange(nb).map( v => (x,y+v))

    def closestPoints(p1:String, p2: String) : Seq[(Int,Int)] = {
        val p1p = parse(p1)
        val p2p = parse(p2)
        closestPoints(p1p,p2p)
    }
 
    def closestPoints(p1:Cmds, p2:Cmds) : Seq[(Int,Int)] = {
        //println("p1=" + generatePoints(p1))
        //println("p2=" + generatePoints(p2))
        val points = generatePoints(p1).toSet.intersect(generatePoints(p2).toSet) 
        points.toSeq.sortWith((p1,p2) => Math.abs(p1._1) + Math.abs(p1._2) < Math.abs(p2._1) + Math.abs(p2._2))
    }

    def getSignalDelay(c1:Cmds, c2:Cmds) = {
        val p1 = generatePoints(c1)
        val p2 = generatePoints(c2)
        val interSection = p1.toSet.intersect(p2.toSet)
        interSection.map( p => (p1.indexOf(p) + p2.indexOf(p) + 2)).toSeq.sorted
    }
}