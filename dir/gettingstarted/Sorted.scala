object Sorted {

   def isSorted[A]( as: Array[A], ordered: (A,A) => Boolean ): Boolean = {
      @annotation.tailrec
      def loop( n : Int ) : Boolean = {
         
         if (n >= as.size) return true
         else {
            if ( ordered( as(n-1), as(n) )) {
               loop(n+1)
            }
            else {
               return false
            }

         }

      }

      loop(1)

   }

   def main(args:Array[String]) : Unit =
   {

      val a = Array(1,2,0,5,10,11)

      println("is sorted? " + isSorted(a, (one : Int, two: Int)=> one < two ))

   }

}
