import ziv.datastruct._

object RunStruct {
  def tail[A](l: List[A]): List[A] = 
   l  match {

      case Nil => Nil
      case Cons(x, xs) => xs
  
  }

  def setHead[A]( h: A, l: List[A] ) : List[A] =
    l match {

      case Nil => Nil
      case Cons(_, t) => Cons(h, t)
   
    }

  def drop[A](l: List[A], n: Int) : List[A] =
  {
    if (n == 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n-1)
    }
    
    }
   }

   def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
   {
     l match {

       case Nil => Nil
       case Cons(h, t) if f(h) => dropWhile(t, f)
       case l => l

     } 
   }

   def init[A](l: List[A]): List[A] = {

      l match {

         case Nil => Nil
         case Cons(_, Nil) => Nil
         case Cons(h, t) => Cons(h, init(t))
      }

   }

   def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B) : B =
      as match {

         case Nil => z
         case Cons(h, t) => f(h, foldRight(t, z)(f))

      }
  
   def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = 
   {
      as match {
         case Nil => z
         case Cons(h,t) => foldLeft(t, f(z,h))(f)
      }

   }

   /*
   def foldLeftViaRight[A,B](as: List[A], z: B)(f: (B, A) => B): B =
   {

      // see https://github.com/fpinscala/fpinscala/blob/master/answerkey/datastructures/13.answer.scala

   } 
   */
   def append[A](a1: List[A], a2: List[A]) : List[A] = 
   {

      foldRight(a1, a2)((a, b) => Cons(a, b))

   }

   def append2[A](a1: List[A], a2: List[A]) : List[A] =
   {
      // need to call reverse on a1
      foldLeft(a1, a2)((a, b) => Cons(b, a))

   }

   def concat[A]( l: List[List[A]]) : List[A] =
   {
      
      foldRight(l, List[A]())((a, b) => append(a, b))

   } 

   def addOne( l: List[Int]) : List[Int] = 
   {

      foldRight(l,Nil : List[Int] )((a, b) => Cons(a+1, b))

   }

   def toString( l : List[Double] ) : List[String] = 
   {

      foldRight(l, Nil : List[String] )((a, b) => Cons(a.toString, b))

   }
 
   def length[A](as: List[A]) : Int =
   {

      val len = foldRight(as, 0)((_,y) => y+1)
      len
   }

   def map[A, B](as : List[A])(f: A => B) : List[B] =
   {

      foldRight(as, Nil : List[B] )((a, b) => Cons(f(a), b))


   }


   def filter[A](as: List[A])(f: A => Boolean): List[A] = 
   {

      foldRight(as, Nil:List[A])((a, b) => if ( f(a) ) Cons(a, b) else b) 

   }

   def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
   {

      val LoL = foldRight(as, Nil: List[List[B]])( (a, b) => Cons( f(a), b ) )
      concat( LoL )
   }

   def filter2[A](as: List[A])(f: A => Boolean): List[A] =
   {

      flatMap(as)(i => if (f(i)) List(i) else List())

   } 

   def addList( l1 : List[Int], l2 : List[Int] ) : List[Int] = 
   {
      (l1, l2) match {
           case (Nil, Nil) => Nil
           case (Nil, _) | (_, Nil) => Nil 

           case ( Cons(h1,t1), Cons(h2, t2) ) => Cons( h1 + h2, addList(t1, t2) ) 

      } 

   }

   
   def zipWith[A, B]( l1 : List[A], l2 : List[A] )(f: (A, A) => B) : List[B] = {
      (l1, l2) match {
           case (Nil, Nil) => Nil
           case (Nil, _) | (_, Nil) => Nil 

           case ( Cons(h1,t1), Cons(h2, t2) ) => Cons( f(h1, h2), zipWith(t1, t2)(f) ) 

      } 

   } 
   
   def subRecur[A](l1 : List[A], l2 : List[A]) : Boolean =
   {
      println("l1: " + l1)
      println("l2: " + l2)
      //println( sub )
      println("___________________")
      (l1, l2) match {
         
         case (_, Nil) => true 
         //case (Nil, _) => false
         case (Cons(h1,t1), Cons(h2, t2))  if (h1 == h2) => subRecur(t1, t2)
         case _ =>  false //hasSubsequence(t1, sub)
      
     }
   }      

   def hasSubsequence[A](sup: List[A], sub: List[A]) : Boolean = 
   {
      println(sup)
      sup match {
         case Nil => sub == Nil
         case _ if subRecur(sup, sub) => true
         case Cons(_, t) => hasSubsequence(t, sub)

      }
      

   }


   def main(args: Array[String]) : Unit = {
     val x = List(1,2,3,4,5) match {
       case Cons(x, Cons(2, Cons(4, _))) => x
       case Nil => 42
       case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
       case Cons(h, t) => h + List.sum(t)
       case _ => 101
     }
     println( x )

     val l = tail(List(1,4,5,6,9))

     val l2 = setHead(20, List(1,4,5,6,9))
     val l3 = drop(List(1,2,3,4,5), 3)
     val l4 = dropWhile(List(1,2,3,4,5,6), (a: Int) => a < 4)
     val l5 = init(List(1,2,3,4))
     //print( l5 )
      
     val test = foldRight( List(1,2,3,4), Nil:List[Int])(Cons(_,_))
     //println( test )

      val len = length(List(1,2,3,4,5))
      //println("len: " + len) 
      
      val appendL = append2(List(1,2,3), List(4,5,6))
      //println(appendL)
      val LoL = List(List(1,2,3), List(4,5,6), List(7,8,9))
      val LoLCat = concat( LoL )
      //println( LoLCat )

      val addL = addOne( List(1,2,3,4) )
      //println( addL ) 

      val strL = toString( List(0.0, 3.3, 4.55) )
      //println( strL )

      val filtL = filter( List(0,1,2,3,5,7,4) )( a => a % 2 == 0 )
      //println( filtL )       
      //println( 4 % 2)

      //println( flatMap(List(1,2,3))(i => List(i,i)) )

      println( hasSubsequence( List(1,2,3,4,5), List(3,4)))
      println( hasSubsequence( List(1,2,3,4,5), List(1,4)))


   }

}

