object Day3 {
    //import scala.io.Source
    //val i3 = Source.fromURL("https://adventofcode.com/2019/day/3/input")
    val s1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    val p1 = "R8,U5,L5,D3"
    val p2 = "U7,R6,D4,L4"


    def main(args: Array[String]): Unit = {
        println(parse(s1))
        println(getMinMax(parse(s1),'R','L'))

        val p1p = parse(p1)
        val p2p = parse(p2)
        println(getMinMax(p1p))
        println(getMinMax(p2p))
        val g = new Grid(getMinMax(p1p))
        g.run(p1p)
        g.print()
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
    def getMinMax(c:Cmds, plus:Char,minus:Char) : (Int,Int) =  
    {
        val r = c.flatMap( 
            (cmd,v) => if (cmd == plus ) List(v) else if (cmd == minus) List(-v) else List() 
            // accumulator (min,max,value)
            ).foldLeft((0,0,0))((a,p) => {
                val np = a._3 + p
                (Math.min(a._1,np), Math.max(a._2,np),np)
            })
        (r._1,r._2)
    }

    // ((minX,maxX),(minY,maxY))
    type Rectangle = ((Int,Int),(Int,Int))
    def getMinMax(c:Cmds) : Rectangle = {
        val xMinMax = getMinMax(c,'R','L')
        val yMinMax = getMinMax(c,'U','D')
        (xMinMax,yMinMax)
    }

    class Grid(r:Rectangle) {
        val ((minX,maxX),(minY,maxY)) = r
        val v = Array.ofDim[Int](maxX-minX+1,maxY-minY+1)

        def traceX(p1:(Int,Int), vx:Int) = {
            if (vx >= 0) {
                for (x <- p1._1 to p1._1 + vx) {
                    v(x)(p1._2) = 1
                }
            } else { 
                for (x <- p1._1 + vx to p1._1) {
                    v(x)(p1._2) = 1
                }
            }
            (p1._1 + vx, p1._2)
        }

        def traceY(p1:(Int,Int), vy:Int) = {
            if (vy >=0 ) {
                for (y <- p1._2 to p1._2 + vy) {
                    v(p1._1)(y) = 1
                }
            } else {
                for (y <- p1._2 + vy to p1._2) {
                    v(p1._1)(y) = 1
                }
            }
            (p1._1, p1._2 + vy)
        }

        def run(cmds:Cmds) = cmds.foldLeft((0,0))(interpret)

        def interpret(p:(Int,Int), c:(Char,Int)) = 
            c._1 match {
                case 'R' => traceX(p,c._2)
                case 'L' => traceX(p,-c._2)
                case 'U' => traceY(p,c._2)
                case 'D' => traceY(p,-c._2)
            }
        
        def print() = for (y <- 0 to maxY- minY) {
            val l = (0 to maxX-minX).map(v(_)(y))
            println(l)     
            }


    }

}