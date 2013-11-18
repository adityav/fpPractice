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
    series of (Coin, Turn)*3 $a1"
  """
  val machine1 = Machine(true,10, 10)
  val machine2 = Machine(false,10, 10)
  def a1 = {
    val inputs:List[Input] = List.fill(3)(Coin::Turn::Nil).flatten
    val t = simulateMachine(inputs)

    val (finalMachine,_) = t run machine1

    finalMachine mustEqual Machine(true, 7, 13)

  }


}
