import org.specs2._

/**
 * User: Aditya Vishwakarma
 * Date: 05/11/13
 * Time: 2:35 PM
 */
class Chapter4tests extends Specification  {
  def is = s2"""
  Tests for Chapter 4 Example code

  for Option
    map2 should
      option + option => Option $m1
      option + None => None     $m2
      none + none => None       $m3
    traverse should
      Some[List] for valid list $e1
      None for any other        $e2

  for Either
    map should map over right, pass left         $k1
    flatMap should flatmap over right, pass left $k2
    orElse should work correctly    $k3
    map2 should work correctly      $k4
    traverse should work        $k5

  """

  def map2func[A,B](a:A ,b: B) = a :: b :: Nil

  def m1 = {
    Chapter4.map2(Some('a'), Some(1))(map2func) mustEqual Some('a' :: 1 :: Nil)
  }
  def m2 = {
    Chapter4.map2(Some('a'), None:Option[Int])(map2func).isEmpty and
      Chapter4.map2(None:Option[Char], Some(1))(map2func).isEmpty
  }
  def m3 = {
    Chapter4.map2(None:Option[Char], None:Option[Int])(map2func).isEmpty
  }


  def e1 = {
    val l1:List[Int] = (1 to 5).toList
    Chapter4.traverse(l1)(a => Some(a)) mustEqual Some(l1)
  }

  def e2 = {
    val l1 = List(1, 6, 3, 4, 5)
    Chapter4.traverse(l1)(a => if(a == 6) None else Some(a)).isEmpty
  }

  //Either
  val testER:Chapter4.Either[String, Int] = Chapter4.Right(1)
  val testEL:Chapter4.Either[String, Int] = Chapter4.Left("Error!")

  def k1 = testER.map(_ + 1) must beEqualTo(Chapter4.Right(2)) and
    (testEL.map(_ + 1) must beEqualTo(testEL))

  def k2 = testER.flatMap(x => Chapter4.Right(x + 1)) must beEqualTo(Chapter4.Right(2)) and
  (testEL.flatMap(x => Chapter4.Right(x + 1)) must beEqualTo(testEL))

  def k3 = testER.orElse(testEL) mustEqual testER and (testEL.orElse(testER) mustEqual testER)

  def k4 = testER.map2(testEL)(_ + _) mustEqual testEL and
    (testER.map2(testER)(_ + _) mustEqual Chapter4.Right(2)) and
    (testEL.map2(testER)(_ + _) mustEqual testEL)

  def k5 = {
    val l1 = (1 to 5).toList
    val expected = Chapter4.Right(l1.map(_ + 5))
    //Success case and failure case
    Chapter4.traverseE(l1)(x => Chapter4.Right(x + 5)) mustEqual expected and
      (Chapter4.traverseE(l1)(x =>
        if(x == 2)
          Chapter4.Left("4 isn't what i want")
        else
          Chapter4.Right(x + 5)) mustEqual Chapter4.Left("4 isn't what i want"))
  }
}

