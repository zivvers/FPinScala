object Curry {

   def curry[A,B,C](f: (A,B) => C): A => (B => C) =
   {
      (a: A) => (b: B) => f(a: A, b: B) 

   }

   def uncurry[A,B,C](f: A => B => C) : (A, B) => C =
   {
   
      (a: A, b: B) => f(a:A)(b:B)

   }

   def compose[A,B,C](f: B => C, g: A => B): A => C =
   {

      (a: A) => f(g( a ))

   }

}
