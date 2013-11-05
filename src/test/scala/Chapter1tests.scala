/**
 * Created with IntelliJ IDEA.
 * User: aditya
 * Date: 4/11/13
 * Time: 7:26 PM
 * To change this template use File | Settings | File Templates.
 */

import org.specs2._
class Chapter1tests extends Specification  {
  def is = s2"""
  Tests for Chapter 1 Example code

  isSorted should
    true for sorted list      $e1
    false for any other       $e2
    true for Empty List       $e3
    true for List with one element $e4 """
  def e1 = {
    val l1 = List(1, 2, 3, 4, 5).reverse
    val l2 = List('a', 'b','c','d').reverse
    Chapter1.isSorted(l1, (a:Int,b:Int) => a > b) and Chapter1.isSorted(l2, (a:Char,b:Char) => a > b)
  }

  def e2 = {
    val l1 = List(1, 6, 3, 4, 5).reverse
    val l2 = List('a', 'j','c','d').reverse
    !Chapter1.isSorted(l1, (a:Int,b:Int) => a > b) and !Chapter1.isSorted(l2, (a:Char,b:Char) => a > b)
  }

  def e3 = Chapter1.isSorted[Int](List.empty[Int], _ > _)
  def e4 = Chapter1.isSorted[Int](List(1), _ > _)

}

class Chapter4tests extends Specification  {
  def is = s2"""
  Tests for Chapter 4 Example code

  map2 should
    option + option => Option $m1
    option + None => None     $m2
    none + none => None       $m3


  traverse should
    Some[List] for valid list      $e1
    None for any other       $e2 """

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
    val l1 = List(1, 2, 3, 4, 5)
    Chapter4.traverse(l1)(a => Some(a)) mustEqual Some(l1)
  }

  def e2 = {
    val l1 = List(1, 6, 3, 4, 5)
    Chapter4.traverse(l1)(a => if(a == 6) None else Some(a)).isEmpty
  }
}
