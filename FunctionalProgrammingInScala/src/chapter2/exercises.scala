package chapter2

import scala.annotation.tailrec

object exercises {

  def main(args: Array[String]): Unit = {
    println(fib(5)) //returns 3
    println(isSorted(Array[Int](1,2,3,4,5,6), ((x:Int,y:Int) => x <=y))) // returns true
    println(isSorted(Array[Int](1,2,3,4,7,5,6), ((x:Int,y:Int) => x <=y))) // returns false
    
    val c = curry((a:Int, b:Int) => a+b)
    val add4 = c(4) // our new function that adds 4 to whatever is passed to it
    println(add4(8)) // should be 12
    println(add4(4)) // should be 8
    println(add4(2)) // should be 6
  }

  /**
   * 2.1 A recursive function to get the nth Fibonacci number.
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
     * 2.2 A recursive function that checks an array is in sorted order
     */
    def isSorted[A](as : Array[A], f: (A,A) => Boolean): Boolean = {
      def loop(n: Int): Boolean = {
        if(n >= as.length -1) true //got to the end
        else if (f(as(n), as(n+1))) loop(n+1) //still sorted so keep going
        else false //f() has returned false, it's all over
      }
      loop(0)
    }
    
    /**
     * 2.3 Currying function that converts a function of 2 args into a function of one arg that partially applies the first
     */
    def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
      a => b => f(a, b)
    }
      
    /**
     * 2.4 Reverse the transformation of curry
     */
    def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
      (a,b) => f(a)(b)
    }
}