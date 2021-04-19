val input = scala.io.Source.fromResource(s"2020/day08.txt").getLines().toList

case class Operation(op: String)
case class Argument(arg: Int)
case class Instruction(op: Operation, arg: Argument)

def parseOperation(rawOperation: String) = {
  rawOperation match {
    case "acc" | "jmp" | "nop" => Some(Operation(rawOperation))
    case _ => None
  }
}

def parseArgument(rawArgument: String) = {
  try {
    val sign = rawArgument.charAt(0)
    val num = rawArgument.slice(1, rawArgument.length()).toInt

    sign match {
      case '+' => Some(Argument(num))
      case '-' => Some(Argument(-num))
      case _ => None
    }
  } catch {
    case e: Exception => None
  }
}

def parseInstruction(rawInstruction: String) = {
  val opAndArg = rawInstruction.split(' ')
  val op = parseOperation(opAndArg(0))
  val arg = parseArgument(opAndArg(1))
  (op, arg) match {
    case (Some(x), Some(y)) => Some(Instruction(x, y))
    case _ => None
  }
}

def solve(instructions: List[Option[Instruction]]) = {
  @annotation.tailrec
  def go(accumulator: Int, instructionIdx: Int, visited: Set[Int]): Int = {
    if (visited.contains(instructionIdx)) {
      accumulator
    } else {
      instructions(instructionIdx) match {
        case Some(x) => {
          x.op.op match {
            case "acc" => go(accumulator+(x.arg.arg), instructionIdx+1, visited+instructionIdx)
            case "jmp" => go(accumulator, instructionIdx+(x.arg.arg), visited+instructionIdx)
            case "nop" => go(accumulator, instructionIdx+1, visited+instructionIdx)
          }
        }
        case _ => go(accumulator, instructionIdx, visited+instructionIdx)
      }
    }
  }
  go(0, 0, Set())
}

def solve2(instructions: List[Option[Instruction]]) = {
  def go(accumulator: Int, instructionIdx: Int, changed: Boolean, visited: Set[Int]): Option[Int] = {
    if (instructionIdx == instructions.length) Some(accumulator)
    else if (instructionIdx > instructions.length || visited.contains(instructionIdx)) None
    else {
      instructions(instructionIdx) match {
        case Some(x) => {
          x.op.op match {
            case "nop" => {
              if (changed) {
                go(accumulator, instructionIdx+1, changed, visited+instructionIdx)
              } else {
                go(accumulator, instructionIdx+x.arg.arg, true, visited+instructionIdx)
                  .orElse(go(accumulator, instructionIdx+1, changed, visited+instructionIdx))
              }
            }
            case "jmp" => {
              if (changed) {
                go(accumulator, instructionIdx+x.arg.arg, changed, visited+instructionIdx)
              } else {
                go(accumulator, instructionIdx+1, true, visited+instructionIdx)
                  .orElse(go(accumulator, instructionIdx+x.arg.arg, changed, visited+instructionIdx))
              }
            }
            case "acc" => go(accumulator+x.arg.arg, instructionIdx+1, changed, visited+instructionIdx)
          }
        }
      }
    }
  }
  go(0, 0, false, Set()).getOrElse(0)
}

val instructions = input.map(parseInstruction)
solve(instructions)
solve2(instructions)