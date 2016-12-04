package chapter3

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil         => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

    /**
     * 3.2 Tail of a List
     */
  def tail[A](lst: List[A]): List[A] = {
    lst match {
        case Nil => lst
        case Cons(x, xs) => xs
    }
  }
  
  /**
   * 3.3 Change the head of a List
   */
  def setHead[A](lst: List[A], a: A): List[A] = {
    lst match {
      case Nil => lst
      case _ => Cons(a, tail(lst))
    }  
  }
  
  /**
   * 3.4 Generalized version of drop which removes the first n elements
   */
  @tailrec
  def drop[A](lst: List[A], n: Int): List[A] = {
    n match {
      case 0=> lst
      case _ => drop(tail(lst), n-1)
    }
  }
  
  /**
   * 3.5 Removes elements from a list as long as they match a predicate
   */
  def dropWhile[A](lst: List[A], f: A => Boolean): List[A] = {
    lst match {
      case Nil => lst
      case Cons(x, xs) => if(f(x)) dropWhile(xs, f) else lst 
    }
  }
  
  /**
   * 3.6 Returns all but last element of a List 
   */
  //@tailrec - not tail recursive so will throw a StackOverflowError on a large list
  def init[A](lst: List[A]): List[A] = {
    lst match {
      case Nil => lst
      case Cons(x,Nil) => Nil
      case Cons(x,xs) => Cons(x,init(xs))     
    }
  }
  
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }
  
  def sum2(lst: List[Int]) = {
    foldRight(lst, 0)(_+_)
  }
  
  def product2(lst: List[Double]) = {
    foldRight(lst, 1.0)(_*_)
  }
  
  /**
   * 3.9 Length of a list using foldRight
   */
  def length[A](lst: List[A]): Int = {
    foldRight(lst, 0)((x,y) => y+1)
  }
    
  /**
   * 3.10 FoldLeft tail recursive
   */
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }
  
  /**
   * 3.11 Implement sum, product, and length using foldLeft
   */
  def sum3(lst: List[Int]) = {
    foldLeft(lst, 0)(_+_)
  }
  
  def product3(lst: List[Double]) = {
    foldLeft(lst, 1.0)(_*_)
  }
  
  def length3[A](lst: List[A]): Int = {
    foldLeft(lst, 0)((x,y) => x+1)
  }
  
  /**
   * 3.12 Reverse a list, try using fold
   */
  def reverse[A](lst:List[A]) = {
    foldLeft(lst, List[A]())((x,y)=> Cons(y, x))
  }
}