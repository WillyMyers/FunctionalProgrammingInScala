package chapter2

import scala.annotation.tailrec

object exercises {

  def main(args: Array[String]): Unit = {
    println(fib(5))
  }

  /**
   * A recursive function to get the nth Fibonacci number.
   * First 2 are 0 and 1 and the nth is always the sum of the previous 2 - the sequence begins 0,1,1,2,3,5
   */
    def fib(n: Int) = {
      @tailrec
      def rec(n: Int, a: Int, b: Int): Int = {
        n match {
          case 0 => -1
          case 1 => b
          case _ => rec(n - 1, a + b, a)
        }
      }
      rec(n, 1, 0)
    }
}