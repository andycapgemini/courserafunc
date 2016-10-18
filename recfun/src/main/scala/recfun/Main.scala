package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c==r || c==0) 1
      else pascal(c,r-1) + pascal(c-1,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def count(sum:Int, c: List[Char]):Boolean = {

        if (sum<0) false
        else if (c.isEmpty) sum == 0
        else count(valueOf(sum, c.head),c.tail)
      }

      def valueOf(s:Int,char:Char):Int = {
        if (char == '(') s+1
        else if (char == ')') s-1
        else s
      }
      count(0,chars)
    }
  
  /**
   * Exercise 3
   */
  def countChange(money:Int, coins:List[Int]): Int = {

      if (coins.isEmpty || money<0) return 0

      if (money==0) return 1
      // this was really hard!
      countChange(money-coins.head,coins) + countChange(money,coins.tail)
  }

}
