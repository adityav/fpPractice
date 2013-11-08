/**
 * User: Aditya Vishwakarma
 * Date: 06/11/13
 * Time: 4:03 PM
 * Note: 
 */

import org.specs2._

class Chapter6tests extends Specification {
  import Chapter6.State._
  def is = s2"""
  Test for Vendor Machine
    series of (Coin, Turn)*3 a1"
  """
  def a1 = {
    val inputs = List.fill(3)(Coin)//intersect(List.fill(3)(Coin))
    true

  }


}
