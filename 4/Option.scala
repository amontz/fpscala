
// hide Scala lib defs, since we overwrite
import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case Some(a) if !f(a) => None
    case None => None
  }
  def filter2(f: A => Boolean): Option[A] = {
    flatMap(a => if(f(a)) Some(a) else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def Try[A](a: => A): Option[A] = 
    try Some(a)
    catch { case e: Exception => None }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum/xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap((m: Double) => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2a[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a,b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(a),Some(b)) => Some(f(a,b))
  }
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)
  }

  def sequence1[A](a: List[Option[A]]): Option[List[A]] = {
    def exchange(l: List[Option[A]], o: Option[List[A]]): Option[List[A]] = l match {
      case None :: xs => None
      case Some(x) :: xs => exchange(xs, o).flatMap((ll: List[A]) => Some(x :: ll))
      case _ => o
    }
    exchange(a, Some(List()))
  }
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def traverse1[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def append(l: List[A], lo: List[Option[B]]): List[Option[B]] = l match {
      case None :: t => List(None)
      case h :: t => f(h) :: append(t, lo)
      case _ => lo
    }
    sequence(append(a, List()))
  }
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }
}
