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

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def add(col: Int, row: Int): Int = {
      if (col == 0 || col == row) 1
      else add(col - 1, row - 1) + add(col, row - 1)
    }
    add(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalance(chars: List[Char], count: Int): Boolean = {
      def check(chars: List[Char], count: Int): Boolean = {
        if (chars.isEmpty) { if (count == 0) true else false }
        else if (chars.head == '(') isBalance(chars.tail, count + 1)
        else if (chars.head == ')') isBalance(chars.tail, count - 1)
        else isBalance(chars.tail, count)
      }
      if (count < 0) false else check(chars, count)
    }
    isBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def add(target: Int, list: List[Int], base: Int): Int = {
      if (list.isEmpty) 0
      else {
        val reminder = target % list.head
        if (target == list.head && base == money) {
          1 + add(target, list.tail, base: Int)
        } else if (target == list.head) {
          add(target, list.tail, base: Int)
        } else if (reminder == 0) {
          1 + add(target, list.tail, base: Int) + more(list.head, coins, base) * (base / target)
        } else if (target > list.head) {
          add(reminder, coins, reminder)
        } else add(target, list.tail, base: Int)
      }
    }
    def more(target: Int, list: List[Int], base: Int): Int = {
      if (list.isEmpty) 0
      else {
        val reminder = target % list.head
        if (target == list.head) {
          more(target, list.tail, base)
        } else if (reminder == 0) {
          1 + more(target, list.tail, base) + more(list.head, coins, base) * (target / list.head)
        } else if (target > list.head) {
          amarimore(reminder, coins, reminder)
        } else more(target, list.tail, base)
      }
    }
    def amarimore(target: Int, list: List[Int], base: Int): Int = {
      if (list.isEmpty) 0
      else {
        val amari = target % list.head
        if (money == list.head) {
          1 + amarimore(target, list.tail, base)
        } else if (amari == 0) {
          1 + amarimore(target, list.tail, base) + amarimore(target, list.tail, base) * (base / target) //+ more(list.head, coins)
        } else if (target > list.head) {
          amarimore(amari, coins, base)
        } else {
          amarimore(target, list.tail, base)
        }
      }
    }
    add(money, coins, money)
  }

}
