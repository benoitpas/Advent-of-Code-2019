object Day1 {
    import scala.io.Source
    // returns 400
    //val url = "https://adventofcode.com/2019/day/1/input"
    //val masses = Source.fromURL(url)
    val masses = Source.fromResource("day1.txt").getLines.toList
    def main(args: Array[String]): Unit = {
        val totalFuel1 = masses.foldLeft(0L)((a,m) => a + fuel1(Integer.parseInt(m).toLong))
        println(totalFuel1)
        //println(fuel2(1969))
        val totalFuel2 = masses.foldLeft(0L)((a,m) => a + fuel2(Integer.parseInt(m).toLong))
        println(totalFuel2)
    }
  
    def fuel1(m:Long) = m / 3 - 2

    def fuel2(m:Long) = {
        val s = LazyList.iterate(m)(fuel1)
        s.drop(1).takeWhile(_ > 0L ).foldLeft(0L)(_+_)
    }
  
  }