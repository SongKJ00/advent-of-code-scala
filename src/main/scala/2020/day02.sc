val input = scala.io.Source.fromResource(s"2020/day02.txt").getLines().toList

case class UInt(num: Int)
case class LowerCaseChar(char: Char)
case class Policy(cond1: UInt, cond2: UInt, matchChar: LowerCaseChar)
case class Password(policy: Policy, password: String)

def parseUInt(num: Int): Option[UInt] = {
  if (num >= 0) {
    Some(UInt(num))
  } else {
    None
  }
}

def parseLowerCaseChar(char: Char): Option[LowerCaseChar] = {
  if ((char >= 'a') && (char <= 'z')) {
    Some(LowerCaseChar(char))
  } else {
    None
  }
}

def parsePolicy(policy: String): Option[Policy] = {
  try {
    val digits = policy.split(' ').head.split('-').map(_.toInt)
    val matchChar = policy.split(' ').last.charAt(0)

    val cond1 = parseUInt(digits.head)
    val cond2 = parseUInt(digits.last)
    val char = parseLowerCaseChar(matchChar)
    (cond1, cond2, char) match {
      case (Some(cond1), Some(cond2), Some(char)) => Some(Policy(cond1, cond2, char))
      case _ => None
    }
  } catch {
    case _: Exception => None
  }
}

def parsePart1Password(policy: Policy, password: String): Option[Password] = {
  val matchCount = password.count(_ == policy.matchChar.char)

  if ((matchCount >= policy.cond1.num) && (matchCount <= policy.cond2.num)) {
    Some(Password(policy, password))
  } else {
    None
  }
}

def parsePart2Password(policy: Policy, password: String): Option[Password] = {
  try {
    val firstChar = password.charAt(policy.cond1.num -1)
    val secondChar = password.charAt(policy.cond2.num -1)
    if ((firstChar == policy.matchChar.char) != (secondChar == policy.matchChar.char)) {
      Some(Password(policy, password))
    } else {
      None
    }
  } catch {
    case e: StringIndexOutOfBoundsException => {
      None
    }
  }
}

def parsePassword(parse: (Policy, String) => Option[Password], policyAndPassword: String): Option[Password] = {
  val rawPassword = policyAndPassword.split(": ").last
  val rawPolicy = policyAndPassword.split(':').head

  val policy = parsePolicy(rawPolicy)

  policy match {
    case Some(x) => parse(policy.get, rawPassword)
    case None => None
  }
}


val part1ValidPasswordSize = input.flatMap(parsePassword(parsePart1Password, _)).size
val part2ValidPasswordSize = input.flatMap(parsePassword(parsePart2Password, _)).size