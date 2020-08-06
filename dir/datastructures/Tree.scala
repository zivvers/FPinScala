//package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


}

object TreeRunner {

   def size[A](root : Tree[A]) : Int = {
      
      root match {
         case Branch(left: Tree[A], right: Tree[A]) => 1 + size[A](left) + size[A](right)
         case Leaf(_) => 1
      }
      
   }   

   def maximum(root: Tree[Int]) : Int = {

      root match {

         case Branch(left: Tree[Int], right: Tree[Int]) => maximum(left) max maximum(right)
         case Leaf(a) => a

      }


   }

   def depth[A](root: Tree[A]) : Int = {

      
      def getDepth[A](base: Tree[A], dep : Int) : Int = {

         base match {

            case Branch(left, right ) => getDepth(left, dep+1) max getDepth(right, dep+1) 
            case Leaf(_) => dep

         }
      
      }
      getDepth(root, 0)

   }


   def map[A, B](root: Tree[A])(f: A => B): Tree[B] = 
   {

      root match {

         case Branch(left, right ) => Branch[B]( map(left)(f), map(right)(f) ) 
         case Leaf( a ) => Leaf[B](f(a)) 

      }
       

   }

   //def fold(root: Tree[Int])(f: (a, b) => ) : Int = {
   def fold[A,B](root: Tree[A])(l: A => B)(b: (B,B) => B): B = 
   {
      root match {

         case Branch(left, right) => b( fold(left)(l)(b) , fold(right)(l)(b) )
         case Leaf(a) => l( a )

      }


   }




   def main(args: Array[String]) : Unit = {

      
      println( "hi")      

      val leaf1 : Tree[Int] = Leaf(1)
      val leaf2 : Tree[Int] = Leaf(2)
      
      val tree : Tree[Int] = Branch(leaf1, leaf2) 

      val siz = size(tree)
      //println("size: " + siz)

      val max = maximum(tree)
      
      println("depth: " + depth( tree ))

      val tree2 = map(tree)(a => a +10)

      println("max tree2: " + maximum(tree2))
   }

}
