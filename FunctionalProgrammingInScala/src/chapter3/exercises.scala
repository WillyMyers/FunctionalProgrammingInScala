package chapter3

import chapter3.List._
object exercises {
  
  def main(args: Array[String]): Unit = {
    println(x)
  }
  
  /**
   * 3.1 what will the result be? I think it will be 3...
   */
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x,Cons(y,Cons(3,Cons(4, _)))) => x+y //this will match and result in 3
    case Cons(h,t) => h + sum(t) //this would match and result in 15 but it doesn't get that far
    case _ => 101
  }
}