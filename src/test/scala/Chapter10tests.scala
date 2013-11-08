/**
 * User: Aditya Vishwakarma
 * Date: 07/11/13
 * Time: 10:44 PM
 * Note: 
 */

import org.specs2._
import org.scalacheck._
import Prop.forAll

class Chapter10tests extends Specification with ScalaCheck { def is =
  s2"""
  Chapter 10 Monoid Laws
    String monoid law holds = $s1
    Int Monoid
      addition monoid holds $s2
      multi monoid holds $s3

    List Monoid holds $s4
    Boolean
      or monoid holds $s5
      and monoid holds $s6

    word monoid
      law holds $s7
      correctly works on example list $s8

  """
  import Chapter10._

  def monoidLaws[A](m: Monoid[A])(implicit ev:Arbitrary[A]):Prop = {
    forAll { (s1: A, s2: A, s3: A) => m.op(m.op(s1,s2), s3) == m.op(s1,m.op(s2,s3))} &&
      forAll { (s1: A) => m.op(s1,m.zero) == s1 && (m.op(m.zero, s1) == s1)}
  }

  def s1 = monoidLaws(stringMonoid)
  def s2 = monoidLaws(intAddition)
  def s3 = monoidLaws(intMultiplication)
  def s4 = monoidLaws(listMonoid[Int]) and monoidLaws(listMonoid[Float])
  def s5 = monoidLaws(booleanOr)
  def s6 = monoidLaws(booleanAnd)
  def s7 = monoidLaws(wordsMonoid)

  def s8 = {
    val s1 = List("Hic", "est ",  "chorda ")
    val s2 = List("Hic "," est",  "chorda")
    val result = "Hic est chorda"

    (wordsMonoid.zero /: s1)(wordsMonoid.op) mustEqual result and
      ((wordsMonoid.zero /: s2)(wordsMonoid.op) mustEqual result)
  }

}
