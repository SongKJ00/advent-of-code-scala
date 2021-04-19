val input = scala.io.Source.fromResource(s"2020/day09.txt").getLines().toList.map(_.toLong)

def solve1(data: List[Long]) = {
  @annotation.tailrec
  def go(currData: Long, preamble: List[Long], remainData: List[Long]): Long = {
    if (preamble.intersect(preamble.map(x => currData - x)).nonEmpty) {
      go(remainData.head, preamble.drop(1):+currData, remainData.drop(1))
    } else {
      currData
    }
  }
  go(data(26), data.take(25), data.drop(26))
}

def solve2(data: List[Long], target: Long) = {
  @annotation.tailrec
  def go(start: Int, end: Int): Long = {
    val currGroup = data.slice(start, end)
    val sum = currGroup.sum
    if (sum == target) {
      currGroup.min + currGroup.max
    } else if (sum < target) {
      go(start, end+1)
    } else {
      go(start+1, end)
    }
  }
  go(0, 1)
}

val invalidNumber = solve1(input)
solve2(input, invalidNumber)

