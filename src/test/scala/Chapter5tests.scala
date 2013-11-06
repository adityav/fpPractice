import org.specs2._

/**
 * User: Aditya Vishwakarma
 * Date: 05/11/13
 * Time: 5:50 PM
 * Note: 
 */
class Chapter5tests extends Specification {
  def is = s2"""
  Test for Chapter 5 Exercises
    Stream should
      toList convert stream to a List $s1
      take(n) should take 1st n elements $s2
      takeWhile(p) should work        $s3
      ones and constant work $s4
      stream of numbers $s5
      fibonacci series $s6
      unfold works $s7

    Ex13 onwards
      startsWith works  $s8
      tails works       $s9
    """
  import Chapter5._
  val l = (1 to 5).toList
  def s1 = Stream(l: _*).toList mustEqual (1 to 5).toList

  def s2 = Stream(l: _*).take(2).toList mustEqual (1 to 2).toList and
    (Stream(l: _*).take(10).toList mustEqual l)

  def s3 = Stream(l: _*).takeWhile( _ < 3).toList mustEqual (1 to 2).toList and
    (Stream(l: _*).takeWhile( _ < 10).toList mustEqual l) and
    (Stream(l: _*).takeWhile( _ > 10).toList mustEqual List.empty)

  def s4 = Stream.ones.list5 mustEqual List(1, 1, 1, 1, 1) and
    (Stream.constant(1).list5 mustEqual Stream.ones.list5)

  def s5 = Stream.from(3).list5 mustEqual (3 to 7).toList

  def s6 = Stream.fibs.take(7).toList mustEqual List(0, 1, 1, 2, 3, 5, 8)

  def s7 = Stream.unfold(3)(x => Some(x, x + 1)).list5 mustEqual Stream.from(3).list5

  def s8 = {
    val s = Stream(l: _*)
    Stream.startsWith(s, Stream(1,2)) and !Stream.startsWith(s, Stream(2,3)) and
      !Stream.startsWith(Stream(1,2), s) and !Stream.startsWith(Stream(2,3), s)
  }

  def s9 = {
    Stream(1,2,3).tails.map(_.toList).toList mustEqual List(List(1, 2, 3), List(2, 3), List(3))
  }

}
