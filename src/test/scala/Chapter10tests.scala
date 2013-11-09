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

     foldMap tests
      concatenate $s9
      foldMap $s10
      foldLeft via foldMap $s11
      foldRight via foldMap $s12
      foldMapV test $s15

     isoMorphism test
      length is homorphic from String to Int $s13
      crazy homomorphism test $s14

     monoid Composition
      intAdd and intMul $mc1
      intAdd and String $mc2
      LIst and String $mc3

  """
  import Chapter10._

  def monoidLaws[A](m: Monoid[A])(implicit ev:Arbitrary[A]):Prop = {
    forAll { (s1: A, s2: A, s3: A) => m.op(m.op(s1,s2), s3) == m.op(s1,m.op(s2,s3))} &&
      forAll { (s1: A) => m.op(s1,m.zero) == s1 && (m.op(m.zero, s1) == s1)}
  }

  def isoMorphic[A,B](n:Monoid[A], m:Monoid[B])(f: A => B)(implicit ev:Arbitrary[A]) = {
    forAll{
      (a: A, b: A) =>
        m.op(f(a), f(b)) == f(n.op(a,b))
    }
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

  val l1 = List("Hic", "est",  "chorda")
  val i1 = List(0,1,2)

  def s9 = concatenate(l1, stringMonoid) mustEqual "Hicestchorda"
  def s10 = forAll {
    l : List[String] =>
      val il = (0 until l.size).toList
      foldMap(il, stringMonoid)(x => l(x)) == concatenate(l, stringMonoid)
  }
  def s11 = forAll{
    l : List[String] =>
      val il = (0 until l.size).toList
      foldLeft_withfoldMap(il)("")((acc, i) => l(i) ++ acc) == concatenate(l, stringMonoid)
  }
  def s12 = forAll{
    l : List[String] =>
      val il = (0 until l.size).toList
      foldRight_withfoldMap(il)("")((i, acc) => acc ++ l(i)) == concatenate(l, stringMonoid)
  }

  def s15 = forAll{
    v: Vector[Int] =>
      foldMapV(v, stringMonoid)(_.toString) == foldMap(v.toList, stringMonoid)(_.toString)
  }

  def s13 = isoMorphic(stringMonoid, intAddition)(_.length)

  def s14 = isoMorphic(listMonoid[Int], stringMonoid)(l => ("" /: l.map(_.toString))(_ ++ _))

  //Monoid Composition Proof

  def mc1 = monoidLaws(productMonoid(intAddition, intMultiplication))
  def mc2 = monoidLaws(productMonoid(intAddition, stringMonoid))
  def mc3 = monoidLaws(productMonoid(listMonoid[Int], stringMonoid))

}
