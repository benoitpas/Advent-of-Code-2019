import org.junit.Test
import org.junit.Assert._

class Day10Test {

    val input1 = List(
        ".#..#",
        ".....",
        "#####",
        "....#",
        "...##")

    @Test def testGetPoints = {
        assert(Day10.getPoints(input1) == List((1,0), (4,0), (0,2), (1,2), (2,2), (3,2), (4,2), (4,3), (3,4), (4,4)))
    }

    import Day10.Point
    def assertGetLine(p1:Point,p2:Point, points:List[Point]) = {
        val l1 = Day10.getLine(p1,p2).toSet
        val l2 = Day10.getLine(p2,p1).toSet
        val l3 = points.toSet

        assert(l1 == l3)
        assert(l2 == l3)
    }
    @Test def testGetLine1 = {
        val n = 5
        val p1 = (0,0)
        val p2 = (n,n)
        val points = (0 to n) map ((i) => (i,i))

        assertGetLine(p1,p2,points.toList)
    }

    @Test def testGetLine2 = {
        val n = 5
        val p1 = (0,n)
        val p2 = (n,0)
        val points = (0 to n) map ((i) => (n-i,i))

        assertGetLine(p1,p2,points.toList)
    }

    @Test def testGetLine3 = {
        val n = 5
        val p1 = (1,2*n)
        val p2 = (n,1)
        val points = List((5,1),(5,2),(4,3),(4,4),(3,5),(3,6),(2,7),(2,8),(1,9),(1,10))

        assertGetLine(p1,p2,points.toList)
    }


}