package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int =  t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
  }

  val tt = Branch(Leaf(4), Branch(Leaf(1), Branch(Leaf(3), Leaf(6))

  //def max(t: Tree[Int]) : Int = t match 


  
}
