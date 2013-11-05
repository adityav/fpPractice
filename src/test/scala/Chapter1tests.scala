/**
 * User: Aditya Vishwakarma
 * Date: 4/11/13
 * Time: 7:26 PM
 * Note:
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
    Chapter2.isSorted(l1, (a:Int,b:Int) => a > b) and Chapter2.isSorted(l2, (a:Char,b:Char) => a > b)
  }

  def e2 = {
    val l1 = List(1, 6, 3, 4, 5).reverse
    val l2 = List('a', 'j','c','d').reverse
    !Chapter2.isSorted(l1, (a:Int,b:Int) => a > b) and !Chapter2.isSorted(l2, (a:Char,b:Char) => a > b)
  }

  def e3 = Chapter2.isSorted[Int](List.empty[Int], _ > _)
  def e4 = Chapter2.isSorted[Int](List(1), _ > _)

}

