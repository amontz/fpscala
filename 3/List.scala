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

  def reverse[A](l: List[A]): List[A] = {
    foldRight(l, Nil:List[A])((h: A, z: List[A]) => append(z, List(h)))
  }

  def reverse_FL[A](l: List[A]): List[A] = {
    foldLeft(l, Nil:List[A])((z: List[A], h: A) => append(List(h), z))
  }

  def foldLeft_FR[A,B](as: List[A], z: B)(f: (B,A) => B) = 
    foldRight(as, (b:B) => b)((a, F) => b => F(f(b,a)))(z)

  def foldRight_FL[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b:B) => b)((F, a) => b => F(f(a,b)))(z)

  def append_FR[A](a1: List[A], a2: List[A]): List[A] = 
    foldRight(a1, a2)((a, l) => Cons(a, l))

  def concat[A](ll: List[List[A]]): List[A] =
    foldRight(ll, (l:List[A]) => l)((as, F) => l => append(as, F(l)))(Nil:List[A])

  def concat_FL[A](ll: List[List[A]]): List[A] = 
    foldLeft(ll, (l:List[A]) => l)((F, as) => l => append(F(l), as))(Nil:List[A])

  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => l
    case Cons(h, t) => Cons(h + 1, addOne(t))
  }

  def dToString(l: List[Double]): List[String] = 
    foldLeft(l, Nil:List[String])((sl, h) => append(sl, List(h.toString)))

  def map[A,B](as: List[A])(f: A => B): List[B] = 
    foldLeft(as, Nil:List[B])((nl, h) => append(nl, List(f(h))))

  def filter_bad[A](as: List[A])(f: A => Boolean): List[A] = {
    def addTrue(l: List[A], nl: List[A]): List[A] = l match {
      case Nil => nl
      case Cons(h, t) if f(h) => addTrue(t, Cons(h, nl))
      case Cons(h, t) if !f(h) => addTrue(t, nl)
    }
    addTrue(reverse(as), Nil:List[A])
  }
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def addTrue(h: A, nl: List[A]): List[A] = {
      if (f(h)) Cons(h, nl)
      else nl
    }
    foldRight(as, Nil:List[A])(addTrue)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil:List[B])((l, a) => append(l, f(a)))

  def filter_FM[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil:List[A])
  }

  def add(l1: List[Double], l2: List[Double]): List[Double] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, add(t1, t2))
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
  }
}
