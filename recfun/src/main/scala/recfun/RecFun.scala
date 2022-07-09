package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || r == c) {
      return 1
    }
    else {
      return pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =  {
    var opening_brace = '('
    var closing_brace = ')'
    var opening_brace_count = 0
    var closing_brace_count = 0

    def isBalanced(chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        return opening_brace_count == closing_brace_count
      }
      else if (chars.head == opening_brace) {
        opening_brace_count += 1
      }
      else if (chars.head == closing_brace) {
        if (opening_brace_count <= closing_brace_count) {
          return false
        }
        else {
          closing_brace_count += 1
        }
      }
      return isBalanced(chars.tail)
    }

    return isBalanced(chars)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var m = coins.length
    def change(s: List[Int], m: Int, n: Int): Int = {
      if (n == 0) {
        return 1
      }
      else if (n < 0) {
        return 0
      }
      else if (m <= 0 && n >= 1) {
        return 0
      }

      return change(s, m - 1, n) + change(s, m, n - s(m - 1))
    }
    return change(coins, m, money)
  }
