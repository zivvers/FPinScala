//package ziv.errorhandling

//import ziv.datastruct._


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {



  def map[B](f: A => B): Option[B] = {
      
      this match {

         case None => None
         case Some(a) => Some(f(a))

      }

  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] =
  {

      //(a,b).flatMap{ case(a, b) => f(a, b) }
      a flatMap (aa => b map (bb => f(aa, bb)))
  }


  def getOrElse[B>:A](default: => B): B = {

      this match {

         case None => default
         case Some(a) => a

      }

  }

  def flatMap[B](f: A => Option[B]): Option[B] = {

      map(f) getOrElse None
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
      this map (Some(_)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {

    //if (f(this)) Some(this) else None
    flatMap(a => if (f(a)) Some(a) else None)
  }

  
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

   def mean(xs: Seq[Double]): Option[Double] = {

      if (xs.isEmpty) None
      else Some(xs.sum/xs.length)
   }


   def variance(xs: Seq[Double]): Option[Double] = {

      mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x-m,2))))

   }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = 
  {
   /*   
   //a match {
   //
   //   case Cons(None, t) => return None
   //   case Cons(Some(aa), t) => aa ++ sequence(t)
   //
   //}
   */
      a match {
         case Nil => Some(Nil)
         case h :: t => {println(h); h flatMap (hh => sequence(t) map (hh :: _));}
      }
  }


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  { 
   
      a match {
         case Nil => Some(Nil)
         case h :: t => f(h) flatMap (hh => traverse(t)(f) map( hh :: _))
      }

  }

  def Try[A](a: => A): Option[A] =
  {

   try Some(a)
  
  }

}

object OptionRun {

   def main(args: Array[String]) : Unit = {

      val l = List( Some(1), Some(2), Some(3) )//, Option[Int](2), Option[Int](3))

      val l2 = List(1,2,3,4)

      val l3 = Option.traverse(l2)((a: Int) =>{ try Some( 10/a )
                                                catch { case e: Exception => None }} )
      println( l3 )


   }


}
