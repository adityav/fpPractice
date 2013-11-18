/**
 * User: Aditya Vishwakarma
 * Date: 14/11/13
 * Time: 10:39 PM
 * Note: 
 */

import org.specs2._
import org.scalacheck._
import Prop.forAll
class Chapter12tests extends Specification with ScalaCheck { def is =
  s2"""
  Applicative Tests
    Validation Applicative tryouts $try1
    Webform builder $try2

  Applicative Law


  """
  import Chapter12._

//  def obeysLaw[F[_],A](fa:Applicative[F])(implicit ev:Arbitrary[F[A]]) = {
//    forAll {
//      a:F[A] =>
//      //Identity law
//        fa.apply[A,A](fa.unit(x => x))(a) == a
//        //Need to find a way to function composition here as well.
//        //Homomorphism
//        fa.apply(fa.unit())
//
//    }
//
//  }

  val stringValidator = validationApplicative[String]
  def vempty[A] = Vector.empty[A]

  case class WebForm(name: String, birthyr: Int, phoneNumber: String)

  def getN(a:Int,b:Int, c:Int) = if(a == b && b == c) a else b

  def try1 = {
    def isOdd(n:Int):Validation[String, Int] =
      if(n % 2 != 0) Failure("Int isn't odd",vempty) else Success(n)
    def isMultiple5(n:Int):Validation[String, Int] =
      if(n % 5 != 0) Failure("Int isn't multiple of 5",vempty) else Success(n)
    def isLess10(n:Int):Validation[String, Int] =
      if(n > 10) Failure("Int isn't less than 10",vempty) else Success(n)

    stringValidator.map3(isOdd(10),isMultiple5(10),isLess10(10))(getN) must_== Success(10)
  }
  def try2 = {
    def nameCheck(name:String) = if(name == "Aditya") Success(name) else Failure("Invalid name", vempty)
    def birthdate(date:Int) = if(date < 1990) Success(date) else Failure("Underage", vempty)
    def phoneNumber(pn:String) = if(pn.startsWith("8800")) Success(pn) else Failure("Invalid operator", vempty)

    val name = "Aditya"
    val date = 1987
    val pn = "8800454567"
    stringValidator.map3(nameCheck(name), birthdate(date), phoneNumber(pn))(WebForm.apply) must_== Success(WebForm(name, date, pn))

  }

}
