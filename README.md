fpPractice
==========

Functional Programming in Scala book exercises and test cases

The code under src/main/scala/ChapterXX are exercise solutions
The code under src/test/scala/ChapterXX are test cases built using specs2

Notes:
Lazy Params( b: => T) are evaluated everytime they are used in a function
ListBuffer to List is constant time.

Applicative laws
Composition
apply(map2(f, g)(_compose _))(x) == apply(f)(apply(g)(x))
in essence f(g(x)) == fog(x)

Homomorphism
Passing a func and a value through unit, followed by idomatic application is same as
 passing the result of regular function application through unit
apply(unit(f))(unit(x)) == unit(f(x))

interchange Law
unit should have the same effect whether applied to the first or the second argument of apply
apply(u)(unit(y)) == apply(unit(_(y)))(u)
