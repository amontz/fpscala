 // hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(r) => Right(f(r))
   case Left(e) => Left(e)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(r) => f(r)
   case Left(e) => Left(e)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Right(r) => Right(r)
   case Left(e) => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
   for {
     aa <- this
     bb <- b
   } yield f(aa,bb)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  // how to call this with orElse?
  def inv(x: Int): Either[String, Int] =
    try Right(1/x)
    catch { case e: java.lang.ArithmeticException => Left("Divide by 0!")}

  def Try[A](a: => A): Either[Exception, A] = 
    try Right(a)
    catch { case e: Exception => Left(e) }
}
