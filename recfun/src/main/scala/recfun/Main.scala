package rectum
import common._

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
  def pascal(c: Int, r: Int): Int = if ( r < 2 || c == 0 || c == r) 1 else (pascal(c, r-1) + pascal(c-1, r-1))

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
   
  	def f(chars: List[Char], depth: Integer): Boolean = {
  	  if(chars.isEmpty && depth > 0) false
  		else if(chars.isEmpty && depth == 0) true
  		else if(chars(0) == '(') f(chars.tail, depth + 1)
  		else if(chars(0) == ')') {
  		  if(depth == 0) false
  		  else f(chars.tail, depth - 1)
  		}
  		else f(chars.tail, depth)	
  	
  	}
    f(chars, 0)
         
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def f(money: Int, coins: List[Int], sum: Int): Int = {
      if(coins.isEmpty || money <= 0) 0
      else if(coins(0) == money) 1
      else if(coins(0) > money) sum + f(money, coins.tail, sum)
      else sum + f(money - coins.head, coins, sum)
    }
    
    f(money, coins, 0)
  }    
