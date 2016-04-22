/* 2.1 */

/* 0 1 1 2 3 5 8 13
     | start here
 For n-th term, we do n-1 additions,
 but we loop n times. so always return prev
*/

object Fib {
  def fib(n: Int): Int = {
    def go(n: Int, cur: Int, prev: Int): Int = {
      if (n == 1) prev
      else go(n-1, cur + prev, cur)
    }
    if (n <= 0) -1
    else go(n, 1, 0)
  }
}

/* 2.2 */

object isSorted {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n == as.length) true
      else if (!ordered(as(n-1), as(n))) false
      else loop(n+1)
    }
    if (as.length == 0) true
    else loop(1)
  }
}
