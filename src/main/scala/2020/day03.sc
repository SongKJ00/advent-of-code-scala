val input = scala.io.Source.fromResource(s"2020/day03.txt").getLines().toList

case class UnsignedInt(num: Int)
case class PositiveInt(num: Int)
case class Cell(char: Char)
case class Row(row: List[Cell])
case class TobogganPosition(x: UnsignedInt, y: UnsignedInt)
case class Slope(right: UnsignedInt, down: PositiveInt)
case class Puzzle(rows: List[Row], width: Int, height: Int)

def parseUnsignedInt(num: Int) = {
  if (num >= 0) {
    Some(UnsignedInt(num))
  } else {
    None
  }
}

def parsePositiveInt(num: Int) = {
  if (num > 0) {
    Some(PositiveInt(num))
  } else {
    None
  }
}

def parseCell(char: Char) = {
  val openSquare = '.'
  val tree = '#'
  if (char == openSquare || char == tree) {
    Some(Cell(char))
  } else {
    None
  }
}

def parseRow(rawRow: String) = {
  val row = rawRow.map(parseCell)
  if (row.forall(_.nonEmpty)) {
    Some(Row(row.map(_.get).toList))
  } else {
    None
  }
}

def parseSlope(pos: (Int, Int)) = {
  val right = parseUnsignedInt(pos._1)
  val down = parsePositiveInt(pos._2)

  (right, down) match {
    case (Some(x), Some(y)) => Some(Slope(x, y))
    case _ => None
  }
}

def parsePuzzle(rawRows: List[String]) = {
  val rowsOptions = rawRows.map(parseRow)


  if (rowsOptions.forall(_.nonEmpty)) {
    val rows = rowsOptions.map(_.get)
    val firstRowWidth = rows.head.row.size
    if (rows.forall(_.row.size == firstRowWidth)) {
      Some(Puzzle(rows, firstRowWidth, rows.size))
    } else {
      None
    }
  } else {
    None
  }
}

def parseTobogganPosition(rawX: Int, rawY: Int, puzzle: Option[Puzzle]) = {
  for {
    p <- puzzle
    x <- parseUnsignedInt(rawX)
    y <- parseUnsignedInt(rawY)
    if y.num < p.height
  } yield TobogganPosition(x, y)
}

def slideDown(puzzle: Option[Puzzle], tobogganPosition: Option[TobogganPosition], slope: Option[Slope]) = {
  (puzzle, tobogganPosition, slope) match {
    case (Some(p), Some(t), Some(s)) => {
      def go(acc: Int, currX: Int, currY: Int): Int = {
        val newTobogganPosition = parseTobogganPosition(currX, currY, puzzle)
        if (newTobogganPosition.isEmpty) {
          acc
        } else {
          val nextX = (currX + s.right.num) % p.width
          val nextY = currY + s.down.num

          if (p.rows(currY).row(currX).char == '#') go(acc+1, nextX, nextY) else go(acc, nextX, nextY)
        }
      }

      Some(go(0, t.x.num, t.y.num))
    }
    case _ => None
  }
}

val puzzle = parsePuzzle(input)
val startPosition = parseTobogganPosition(0, 0, puzzle)

val part1Slope = parseSlope((3, 1))
slideDown(puzzle, startPosition, part1Slope).get

val part2Slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).map(parseSlope)
part2Slopes.flatMap(slideDown(puzzle, startPosition, _)).product