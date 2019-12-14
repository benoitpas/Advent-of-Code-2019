object Day6 {
    import scala.io.Source
   
    val input = Source.fromResource("day6.txt").getLines.toList
    val test = List("COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L")

    def strings2Map(strings:List[String]) = strings.foldLeft(Map[String,List[String]]())( (m,s) => {
        val (k,v) = s.split(')').toList match {
            case k::v::_ => (k,v)
        }
        m + ((k,(List(v) ++ m.getOrElse(k,List[String]()))))
    })

    def depths(k:String , m : Map[String,List[String]]) : Integer = {
        m.get(k) match {
            case Some(l) => l.foldLeft(0)((a,k2) => 1 + a + depths(k2,m))
            case _ => 0
        }
    }

    def depths(m : Map[String,List[String]]) : Integer = 
        m.keys.foldLeft(0)((a,k) => a + depths(k,m))

    def main(args: Array[String]): Unit = {

        val m = strings2Map(input)

        println(depths(m))
    }
}