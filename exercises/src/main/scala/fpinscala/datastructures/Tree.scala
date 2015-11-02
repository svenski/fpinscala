package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int =  t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
  }

  val tt = Branch(Leaf(4), Branch(Leaf(1), Branch(Leaf(3), Leaf(6))))

  def max(t: Tree[Int]) : Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => max(l).max(max(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + depth(l).max(depth(r))
  }
  
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =  t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }

//  def fold[A,B](t: Tree[A], z: B)(f: (A,B) => B): B = t match {
//   case Leaf(v) => f(v, z)
//    case Branch(l, r) => fold(r, fold(l, z)(f))(f)
//  }

}
