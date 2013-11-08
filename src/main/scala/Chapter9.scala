/**
 * User: Aditya Vishwakarma
 * Date: 07/11/13
 * Time: 3:31 PM
 * Note: 
 */
object Chapter9 {
  trait Parsers[ParseError, Parser[+_]] {
    self =>

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

    implicit def string(s: String): Parser[String]
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):ParserOps[String] = ParserOps(f(a))

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)



    case class ParserOps[A](p: Parser[A]) {
      def |[B>:A] (p2: => Parser[B]): Parser[B] = self.or(p,p2)
      def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

      def map[B](f: A => B): Parser[B] = ???
      def many: Parser[List[Int]] = ???
    }
  }

}
