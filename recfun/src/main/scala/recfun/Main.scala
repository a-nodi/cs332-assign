package recfun
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

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def iterateList(chars: List[Char], leftCount: Int, rightCount: Int): Boolean = {
      if (chars.isEmpty)
        leftCount == rightCount
      else if (chars.head == '(')
        iterateList(chars.tail, leftCount + 1, rightCount)
      else if (chars.head == ')')
        (leftCount > rightCount) && iterateList(chars.tail, leftCount, rightCount + 1)
      else
        iterateList(chars.tail, leftCount, rightCount)
    }

    iterateList(chars, 0, 0)
  }

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def iterateCoins(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money < 0) // Degenerated case 
        0
      else if (money == 0) // Solution 
        1
      else // Count out coin from money and reuse coin + count out no coin from money and eliminate coin
        iterateCoins(money - coins.head, coins) + iterateCoins(money, coins.tail)
    }

    iterateCoins(money, coins)
  }
}
