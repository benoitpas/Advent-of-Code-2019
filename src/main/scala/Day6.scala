object Day6 {
    import scala.io.Source
   
    val input = Source.fromResource("day6.txt").getLines.toList
    val test = List("COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L")
    val test2 = List("COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN")

    def strings2Map(strings:List[String], reverse: Boolean) = strings.foldLeft(Map[String,List[String]]())( (m,s) => {
        val (k,v) = s.split(')').toList match {
            case k::v::_ => if (!reverse) (k,v) else (v,k)
        }
        m + ((k,(List(v) ++ m.getOrElse(k,List[String]()))))
    })

    // could be memoized if performance issues
    def depths(k:String , m : Map[String,List[String]]) : Integer = {
        m.get(k) match {
            case Some(l) => l.foldLeft(0)((a,k2) => 1 + a + depths(k2,m))
            case _ => 0
        }
    }

    def depths(m : Map[String,List[String]]) : Integer = 
        m.keys.foldLeft(0)((a,k) => a + depths(k,m))

    def distance(from:String, to:String,m2:Map[String,List[String]]) = {
        // Reverse the links in the
        def toCom(from:String) : List[String] = from :: {m2.get(from) match {
            case Some(k) => toCom(k.head)
            case _ => List()
        }}

        val toFrom = toCom(from)
        val toTo = toCom(to)
        val intersection = toFrom.toSet.intersect(toTo.toSet)
        val toFromIndexed = toFrom.toIndexedSeq
        val toToIndexed = toTo.toIndexedSeq

        intersection.map( k => (k,toFromIndexed.indexOf(k) + toToIndexed.indexOf(k) - 2)).toList.sortBy(_._2)
    }
    def main(args: Array[String]): Unit = {

        val m = strings2Map(input,false)
        val m2 = strings2Map(input,true)

        println(depths(m))
        println(distance("YOU","SAN",m2))
    }
}