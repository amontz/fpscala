//package fpinscala.datastructures

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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def head[A](l: List[A]): A = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(h, _) => h
  }

  // returning Nil makes drop cleaner!
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  // works on h not of type A?
  def setHead[A](l: List[A], h: A) = l match {
    case Nil => List(h)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else {
      drop(tail(l), n-1)
    }
  }

  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = {
    if (f(head(l))) dropWhile(tail(l), f)
    else l
  }

  def dropWhile[A](l: List[A], f:A => Boolean): List[A] = l match {
    case Cons(h, t) if(f(h)) => dropWhile(t, f)
    case _ => l
  }

  def length[A](l: List[A]): Int = {
    @annotation.tailrec
    def counter(l2: List[A], n: Int): Int = l2 match {
      case Nil => n
      case Cons(_, t) => counter(t, n+1)
    }
    counter(l, 0)
  }

  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l2: List[A], lcopy: List[A]): List[A] = {
      if (length(lcopy) == 1) l2
      else loop(append(l2, List(head(lcopy))), drop(lcopy, 1))
    }
    loop(List(), l)
  }

  def lengthFR[A](l: List[A]): Int = {
    def counter[A](a: A, len: Int): Int =  {
      1 + len
    }
    foldRight(l, 0)(counter)
  }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z, h))(f)
  }

  def sumFL(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productFL(l: List[Int]): Int = foldLeft(l, 1)(_ * _)
  def lengthFL[A](l: List[A]): Int = foldLeft(l, 0)((c, _) => c+1)
}
