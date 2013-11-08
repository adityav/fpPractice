/**
 * User: Aditya Vishwakarma
 * Date: 07/11/13
 * Time: 1:57 PM
 * Note: 
 */

//Test suite dsl
object Chapter8 {

  trait Prop {
    def check: Unit = ???
    def && (that: Prop): Prop
  }

}
