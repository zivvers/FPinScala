object Fib {

   def fib(n: Int) : Int = {


      if ( n == 0 )
      {
         return 0
        
      }
      if ( n == 1)
      {
         return 1
      }
      @annotation.tailrec
      def go(n: Int, first: Int, second: Int) : Int = {

         if (n == 0)
            return second

         go(n-1, second, first+second)
      
      
      }
      
      return go(n, 0, 1)

   }
   def main(args: Array[String]) : Unit = {

      println("fib: " + fib(3))


   }


}
