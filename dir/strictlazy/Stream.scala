package ziv


import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  final def forAll(p: A => Boolean): Boolean = {

    foldRight(true)( (a, b) => p(a) && b )

  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }


  def take(n: Int): Stream[A] = {

    this match {
      
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty

    }


  }

  def drop(n: Int): Stream[A] = {

    this match {
      
      case Cons(_, t) if n > 0 =>  t().drop(n - 1)
      case _ => this

    }


  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    
    this match {
      
      case Cons(h, t) if p(h()) =>  cons( h(), t().takeWhile(p) )
      case _ => empty

    }


  }

  def takeWhile2(p: A=> Boolean): Stream[A] = {

    foldRight(Stream[A]())( (a,b) => ( 
      
      if (p(a)) cons( a, b )
      else empty 

    ) ) 

  }

  def headOption: Option[A] = {

    foldRight(None: Option[A])( (a, b) => ( 
      

      Some(a) orElse b

    ) )

  }

  def map[B]( f: A => B  ) : Stream[B] = {
    foldRight(Stream[B]())( (a, b) => cons(f(a), b) )


  }

  def filter( f: A => Boolean  ) : Stream[A] = {
    
    foldRight(Stream[A]())( (a, b) => if ( f(a) ) cons(a, b) else b  )

  }

  def append[B>:A]( s2: => Stream[B]  ) : Stream[B] = {
    
    //foldRight(Stream[A]())( (a, b) => if ( f(a) ) cons(a, b) else b  )
    foldRight( s2 )( (a, b) => cons(a, b))

  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {

    foldRight( Stream[B]() )( (a, b) => f(a) append b)

  }

  def toList: List[A] = {

    this match {
      
      case Cons(h, t) => h() :: t().toList
      case _ => List()

    }


  }


  def constant[A](a: A): Stream[A] = {

    //val rep : Stream[A] = Stream.cons(a, rep)
   lazy val tail: Stream[A] = Cons(() => a, () => tail)
   tail 

  }

  def from(n : Int): Stream[Int] = {

   //lazy val a: Stream[Int] = Cons(() => n , () => from(n+1))
   //a
   cons(n, from(n+1)) 

  }

  def fibs() : Stream[Int] = {
     def go(f0: Int, f1: Int) : Stream[Int] =
        cons(f0, go(f1, f0+f1))
     go(0,1)

  }
  
  def unfold[A,S](z: S)(f: S=> Option[(A,S)]): Stream[A] = 
  {
      f( z ) match {

         case Some((h,s)) => cons(h, unfold(s)(f))
         case None => empty

      }

  }

  def fibs2() : Stream[Int] = {

   unfold[Int, (Int, Int)]((0,1))((tup) => Some((tup._1 , (tup._2, tup._1 + tup._2))))

  }

 def from2(n: Int) : Stream[Int] = {

   unfold[Int, Int](n)((num) => Some((num, num + 1)))

 }

 def constant2(n : Int) : Stream[Int] = {

   unfold[Int, Int](n)((num) => Some((num, num )))
 
 }

 def map2[B]( f: A => B  ) : Stream[B] = {

  unfold(this) {
    case Cons(h,t) => Some((f(h()), t()))
    case _ => None
  }

  }

  def take2(n: Int): Stream[A] = {

    unfold((this, n: Int))  {
      
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case (Cons(h, _), 1) if n == 1 => Some((h(), (empty,0)))
      case _ => None

    }
  }

 def takeWhile3(p: A => Boolean): Stream[A] = {
   
   unfold(this) {

     case Cons(h, t) if p(h()) => Some(h(), t())
     case _ => None

   }

 
 }


 def zipWith[A, B]( s1 : Stream[A], s2 : Stream[A] )(f: (A, A) => B) : Stream[B] = {

   unfold( s1, s2 ) {

      case (Cons(h, t), Cons(h2, t2)) => Some(f(h(), h2()), (t(), t2()))
      case _ => None

   }
 
     /* chp 3 implementation
     (l1, l2) match {
           case (Nil, Nil) => Nil
           case (Nil, _) | (_, Nil) => Nil

           case ( Cons(h1,t1), Cons(h2, t2) ) => Cons( f(h1, h2), zipWith(t1, t2)(f) )

      }
      */
   }


  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {

      unfold(this, s2) {
         
         case (Cons(h,t), Cons(h2, t2)) => Some(((Some(h()), Some(h2())), (t(), t2())))
         case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty[A], t2())))
         case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), empty[B])))
         case _ => None
      }

  }

 def startsWith2[A](s: Stream[A]) : Boolean = {

   this.zipAll(s).takeWhile(! _._2.isEmpty) forAll {

      case (h, h2) => h == h2      

   }
   
 }

 def tails: Stream[Stream[A]] = {

   val s = unfold(this) {
      
      case Cons(h, t) => Some(( Cons(h, t)  , t() ))
      case Empty => None
     

   }
   s append Stream(empty)
 }



  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }



  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}
