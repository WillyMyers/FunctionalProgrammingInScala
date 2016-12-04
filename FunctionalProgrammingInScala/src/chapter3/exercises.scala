package chapter3

import chapter3.List._
object exercises {
  
  def main(args: Array[String]): Unit = {
    println(x)
    val lst = List(1,2,3,4,5,6)
    val lstDbl = List(1.0,2.0,3.0,4.0,5.0,6.0)
    println(tail(lst))
    println(setHead(lst, 10))
    println(drop(lst, 3))
    println(drop(Nil, 0))
    val f =  (x:Int) => x < 3
    println(dropWhile(lst, f))
    println(init(lst))
    println(sum2(lst)) // 21
    println(sum(lst)) // 21
    println(product(lstDbl)) //720
    println(product2(lstDbl)) //720
    
    //3.8
    println("Exercise 3.8: " + foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    
    println(length(lst)) // should print 6
    
    println(sum3(lst)) // 21
    println(product3(lstDbl)) //720
    println(length3(lst)) // should print 6
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