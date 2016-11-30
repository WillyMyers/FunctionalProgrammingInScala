package chapter2

import scala.annotation.tailrec

object exercises {

  def main(args: Array[String]): Unit = {
    println(fib(5))
    println(
        isSorted(Array[Int](1,2,3,4,5,6), ((x:Int,y:Int) => x <=y))
        )
  }

  /**
   * A recursive function to get the nth Fibonacci number.
   * First 2 are 0 and 1 and the nth is always the sum of the previous 2 - the sequence begins 0,1,1,2,3,5
   */
    def fib(n: Int) = {
      @tailrec
      def loop(n: Int, a: Int, b: Int): Int = {
        n match {
          case 0 => -1
          case 1 => b
          case _ => loop(n - 1, a + b, a)
        }
      }
      loop(n, 1, 0)
    }
    
    /**
     * A recursive function that checks an array is in sorted order
     */
    def isSorted[A](as : Array[A], f: (A,A) => Boolean): Boolean = {
      def loop(n: Int): Boolean = {
        if(n >= as.length -1) true
        else if (f(as(n), as(n+1))) loop(n+1)
        else false
      }
      loop(0)
    }
    
}