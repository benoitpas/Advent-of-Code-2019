object Day3 {
    //import scala.io.Source
    //val i3 = Source.fromURL("https://adventofcode.com/2019/day/3/input")
    val s1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    val p1 = "R8,U5,L5,D3"
    val p2 = " U7,R6,D4,L4"


    def main(args: Array[String]): Unit = {
        println(parse(s1))
        println(getMinMax(parse(s1),'R','L'))

        val p1p = parse(p1)
        val p2p = parse(p2)
        val ((xMin,xMax),(yMin,yMax)) = getMinMax2(p1p)
        println((xMin,xMax),(yMin,yMax))
    }

    type Cmds =  List[(Char,Int)]
    def parse(s:String) : Cmds = s.split(",").map( e => (e.charAt(0),Integer.parseInt(e.drop(1)))).toList
    /*
    // returns (minX,maxX,minY,maxY) for the list of commands starting from 0,0
    def getMinMax(c:Cmds)/*: (Int,Int,Int,Int)*/ = c.map( (c,v) => c match {
        case 'R' => (v,0)
        case 'L' => (-v,0)
        case 'U' => (0, v)
        case 'D' => (0, -v)
    }).foldLeft(((0,0),(0,0,0,0)))( (a,m) => {
        val 
    })
    */
    def getMinMax(c:Cmds, plus:Char,minus:Char) = {c.flatMap( 
        (cmd,v) => if (cmd == plus ) List(v) else if (cmd == minus) List(-v) else List() 
        // accumulator (min,max,value)
        ).foldLeft((0,0,0))((a,p) => {
            val np = a._3 + p
            (Math.min(a._1,np), Math.max(a._2,np),np)
        }).map( a => (a._1,a._2))}

    def getMinMax2(c:Cmds) = {
        val xMinMax = getMinMax(c,'R','L')
        val yMinMax = getMinMax(c,'U','D')
        (xMinMax,yMinMax)
    }
}