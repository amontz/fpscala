/* 2.1 */

object Fib {
  def fib(n: Int): Int = {
    if (n <= 0) -1
    else if (n == 1) 0
    else if (n == 2) 1
    else fib(n-1) + fib(n-2)
  }
}
