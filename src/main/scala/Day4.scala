object Day4 {
    import scala.io.Source
   
    val (min,max) = (231832,767346)
    val nbDigits = 6

    def main(args: Array[String]): Unit = {
        val numbers = generate(2,nbDigits)
        val numbers2digits = numbers.filter( digits => digits.toSet.size < nbDigits )
        val numbersAsInt = numbers2digits.map(listToInt).filter( n => min < n && n < max)
        println("part1="+numbersAsInt.length)
        val numbers2digits2 = numbers filter groupOf2
        val numbersAsInt2 = numbers2digits2.map(listToInt).filter( n => min < n && n < max)
        println("part2="+numbersAsInt2.length)
    }

    def countDigits(l:List[Int]) = l.foldLeft(Map[Int,Int]())((m,d) => m + (d -> (m.getOrElse(d,0) + 1)))
    def groupOf2(l:List[Int]) = countDigits(l).values.toSet.contains(2)
    def listToInt(l:List[Int]) = l.foldLeft(0)((a,d) => a*10+d)

    def generate(d: Int, n: Int) : IndexedSeq[List[Int]] = {
        val digits = d to 9
        if (n <= 0) IndexedSeq[List[Int]](List()) else digits.flatMap( d => generate(d, n -1).map(l => d::l)) 
    }

    

}