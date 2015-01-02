package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, acc) => Cons(x, acc))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((right, left) => f(left, right))

  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) if n > 0 => drop(xs, n-1)
    }
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => Nil
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(x, xs) => buf += x; go(xs)
    }
    go(l)
  }

  def init2[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init2(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((right, left) => b => left(f(b, right)))(z)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(append)

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")

  def mkString[A](l: List[A]): String = l match {
    case Nil => ""
    case Cons(x, Nil) => x.toString
    case Cons(x, xs) => x + ", " + mkString(xs)
  }
}

object DataStructures extends App {
  import List._

  // Exercise 3.1
  // this should match Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  // and print 3 (from 1 + 2)
  println(s"x = $x")

  // Exercise 3.2
  println(s"tail(List(1,2,3)) = ${mkString(tail(List(1,2,3)))}")

  // Exercise 3.3
  println(s"setHead(List(1,2,3), 4) = ${mkString(setHead(List(1,2,3), 4))}")

  // Exercise 3.4
  println(s"drop(List(1,2,3), 0) = ${mkString(drop(List(1,2,3), 0))}")
  println(s"drop(List(1,2,3), 1) = ${mkString(drop(List(1,2,3), 1))}")
  println(s"drop(List(1,2,3), 2) = ${mkString(drop(List(1,2,3), 2))}")
  println(s"drop(List(1,2,3), 3) = ${mkString(drop(List(1,2,3), 3))}")
  println(s"drop(List(1,2,3), 4) = ${mkString(drop(List(1,2,3), 4))}")

  // Exercise 3.5
  println(s"dropWhile(List(1,2,3), _ > 1) = ${mkString(dropWhile[Int](List(1,2,3), _ > 1))}")
  println(s"dropWhile(List(1,2,3), _ < 2) = ${mkString(dropWhile[Int](List(1,2,3), _ < 2))}")
  println(s"dropWhile(List(1,2,3,1,2), _ < 3) = ${mkString(dropWhile[Int](List(1,2,3,1,2), _ < 3))}")
  println(s"dropWhile(List(1,2,3), _ < 4) = ${mkString(dropWhile[Int](List(1,2,3), _ < 4))}")

  // Exercise 3.6
  println(s"init(List(1,2,3)) = ${mkString(init(List(1,2,3)))}")

  // Exercise 3.8
  // we get back the original list
  println(s"foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) = ${foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))}")

  // Exercise 3.9
  println(s"length(List(9,23,34,0)) = ${length(List(9,23,34,0))}")

  // Exercise 3.10
  println(s"foldLeft(List(1,2,3), 0)(_ + _) = ${foldLeft(List(1,2,3), 0)(_ + _)}")

  // Exercise 3.11
  println(s"sum3(List(1,2,5)) = ${sum3(List(1,2,5))}")
  println(s"product3(List(1,2,5)) = ${product3(List(1,2,5))}")
  println(s"length2(List(3,2,1)) = ${length2(List(3,2,1))}")

  // Exercise 3.12
  println(s"reverse(List(1,2,3)) = ${mkString(reverse(List(1,2,3)))}")

  // Exercise 3.13
  println(s"foldLeft(List(a,b,c), _)(_ + _) = ${foldLeft(List("a","b","c"), "")((acc, x) => acc + x)}")
  println(s"foldLeft2(List(a,b,c), _)(_ + _) = ${foldLeft2(List("a","b","c"), "")((acc, x) => acc + x)}")
  println(s"foldRight(List(a,b,c), _)(_ + _) = ${foldRight(List("a","b","c"), "")((x, acc) => acc + x)}")
  println(s"foldRight2(List(a,b,c), _)(_ + _) = ${foldRight2(List("a","b","c"), "")((x, acc) => acc + x)}")

  // Exercise 3.14
  println(s"append2(List(1,2,3),List(4,5,6)) = ${mkString(append2(List(1,2,3), List(4,5,6)))}")

  // Exercise 3.15
  println(s"concat(List(List(1,2,3),List(4,5,6),List(7,8,9))) = ${mkString(concat(List(List(1,2,3), List(4,5,6), List(7,8,9))))}")
}
