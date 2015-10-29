package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  val bb = List(1,2,3,4, 66, -1)

  def setHead[A](l: List[A], h: A): List[A] = 
    Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 1 => tail(l)
    case nn => drop(tail(l), n-1)
  }

  def f(a: Int) = a > 0
  def g(a: Int) = a < 100
  
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) => if(f(x)) dropWhile(xs, f) else l
    case Nil => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => l
    case Cons(x, xs) => init(xs)
    case Nil => Nil
  }

  val e38 = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Int]) = foldRight(ns, 1)(_ * _)

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)( (x,y) => y + 1)
  }


  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f) 
  }

  def sumLeft(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def productLeft(ns: List[Int]) = foldLeft(ns, 1)(_ * _)
  def lengthLeft(ns: List[Int]) = foldLeft(ns, 0) ((acc,x) => acc +1)

  def rev(l: List[Int]) = foldLeft(l, Nil:List[Int])((acc, x) => Cons(x,acc))

  //def rev(l: List[Int]) = foldLeft(l, Nil:List[Int])((acc, x) => Cons(x,acc))

  def foldLeftUsingFoldRight[A,B](l: List[A], z: B) (f: (B, A) => B): B = {
   // the type we will accumulate
    type Acc = B => B

    // initial value (param `z`) for foldRight, of the Acc type
    def initial: Acc = identity // aka (x => x), discuss this

    // function (param `f`) for foldRight, which builds the Acc type
    def step(a_right: A, acc: Acc): Acc = (b_left: B) => acc(f(b_left, a_right))

    // accumulates a function:
    val g = foldRight(l, initial)(step)

    g(z)
  }
  

  //def foldRightUsingFoldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
  //}
  


  def appendFold[A](a1: List[A], a2: List[A]): List[A] = 
    foldRight(a1, a2)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] = 
    foldRight(l, Nil:List[A])(append)

  def addOneTo(l: List[Int]): List[Int] = 
    foldRight(l, Nil:List[Int])( (x, acc) => Cons(x + 1, acc) )

  def ListDblToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])( (x, acc) => Cons(x.toString, acc))
  
  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, Nil:List[B])((x, acc) => Cons(f(x), acc))

  val useMap = map(List(1.2, 2, 3))( x => x.toString )

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])( (x, t) => if(f(x)) Cons(x, t) else t)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    foldRight(as, Nil:List[B])( (x, acc) => append(f(x), acc))

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = 
    flatMap(as)( x => if(f(x)) List(x) else Nil)

  def addLists(l: List[Int], r: List[Int]): List[Int] = (l,r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
  }
  
  def zipWith[A,B,C](l: List[A], r: List[B])( f: (A, B) => C): List[C] = (l,r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)( f))
  }

  // o
  def hasSubsequenceBySergiusz2[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) =>
        val sufficientLength = length(sup) >= length(sub)
        val isPrefix = sufficientLength && foldLeft(zipWith(sup, sub)(_ == _), true)(_ && _)

        isPrefix || hasSubsequenceBySergiusz2(t1, sub)
    }
  }
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (Nil, Nil) => true
      case(Nil, _) => false
      case(_, Nil) => true
      case(Cons(h1, t1), Cons(h2, t2)) => if(h1 == h2) hasSubsequence(t1, t2) else hasSubsequence(t1, sub)
    }
  }

}
