package adrean.aoc2019.utils

object Math {

  @scala.annotation.tailrec
  def gcd(values:Long*):Long =
    if (values.length > 2) gcd(values.drop(1):_*) else if (values(1) == 0) values(0) else gcd(values(1), values(0) % values(1))

  def lcm(values:Long*):Long =
    if (values.length > 2) (values(0) * lcm(values.drop(1):_*)) / gcd(values:_*) else (values(0) * values(1)) / gcd(values:_*)

}
