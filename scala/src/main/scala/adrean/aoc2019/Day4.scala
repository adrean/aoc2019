package adrean.aoc2019

object Day4 {

  def main(args: Array[String]): Unit = {
    // Part 1
    var count = 0
    for (password <- 138241 to 674034) {
      var meetAdjacents = false
      var meetNeverDecrease = true
      val digits = password.toString.map(_.asDigit)
      for (i <- 1 to 5) {
        if (digits(i) < digits(i-1)) meetNeverDecrease = false
        if (digits(i - 1) == digits(i)) meetAdjacents = true
      }
      if (meetNeverDecrease & meetAdjacents) {
        count += 1
      }
    }
    println(s"Part 1 $count")

    // Part 2
    count = 0
    for (password <- 138241 to 674034) {
      var meetAdjacents = false
      var meetNeverDecrease = true
      val digits = password.toString.map(_.asDigit)
      for (i <- 1 to 5) {
        if (digits(i) < digits(i-1)) meetNeverDecrease = false
        if (digits(i) == digits(i-1)) {
          if (i > 1 & i < 5) {
            if (digits(i + 1) != digits(i) & digits(i - 2) != digits(i)) meetAdjacents = true
          } else if (i == 5) {
            if (digits(i -2) != digits(i)) meetAdjacents = true
          } else {
            if (digits(i + 1) != digits(i)) meetAdjacents = true
          }

        }
      }
      if (meetNeverDecrease & meetAdjacents) {
        count += 1
      }
    }
    println(s"Part 2 $count")
  }


}
