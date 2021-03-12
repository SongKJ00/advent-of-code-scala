val expenses = scala.io.Source.fromResource(s"2020/day1.txt").getLines().toList.map(_.toInt).zipWithIndex

val part1Answer = (for {
  (a, i) <- expenses
  (b, j) <- expenses
  if i != j && a+b == 2020
} yield a*b).head

val part2Answer = (for {
  (a, i) <- expenses
  (b, j) <- expenses
  (c, k) <- expenses
  if i != j && j != k && i != k && a+b+c == 2020
} yield a*b*c).head