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
    """
  import Chapter5._
  val l = (1 to 5).toList
  def s1 = Stream(l: _*).toList mustEqual (1 to 5).toList

  def s2 = Stream(l: _*).take(2).toList mustEqual (1 to 2).toList

}
