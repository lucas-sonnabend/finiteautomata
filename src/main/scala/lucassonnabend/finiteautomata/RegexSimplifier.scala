package finiteautomata

/**
  * This object provides a helper function to simplify regexes from the more complex, standard format
  * into a minimal format.
  */
object RegexSimplifier {
  /**
    * Takes a complex regex and simplifies it.
    *
    * The input can be a complex regex. For now this can support character classes <[]>, particularly the following: a-z, A-Z, 0-9 and \s (whitespaces)
    *
    * The simplified regex will only contain the following operations: concat, star<*>, union<|> and brackets <()>.
    */
  def simplify(complexRegex: String): String = {
    val simpleRegex = new StringBuilder()
    var index = 0

    while (index < complexRegex.length()) {
      val currentChar = complexRegex.charAt(index)
      currentChar match {
        case '\\' =>
          val nextChar = complexRegex.charAt(index + 1)
           nextChar match {
            case '*' | '|' | '(' | ')' =>
              simpleRegex.append('\\' + nextChar)
            case _ =>
              simpleRegex.append(nextChar)
          }
          index = index + 2
        case '[' =>
          val closingIndex = complexRegex.indexOf("]", index + 1)
          simpleRegex.append(convertCharacterClass(complexRegex.substring(index + 1, closingIndex)))
          index = closingIndex + 1
        case _ =>
          simpleRegex.append(currentChar)
          index = index + 1
      }
    }
    simpleRegex.result
  }

  val WHITESPACE_REGEX = " |\t"

  private def convertCharacterClass(characterClass: String): String = {
    val output: StringBuilder = new StringBuilder()

    var index: Int = 0
    var previousChar: Option[Char] = None
    while (index < characterClass.length()) {
      val currentChar = characterClass.charAt(index)
      currentChar match {
        case '\\' =>
          characterClass.charAt(index + 1) match {
            case 's' => output.append(s"|$WHITESPACE_REGEX")
            // TODO add more classes here!
            case c: Char => output.append(s"|$c")
          }
          index = index + 2
        case '-' =>
          if (previousChar.isEmpty) throw new ParsingException('-', index, "Invalid character before the dash")
          else {
            val endChar = characterClass.charAt(index + 1)
            val charSequence = previousChar.get to endChar
            val charUnion: String = charSequence.foldLeft("")((res: String, nextChar:Char) => res+"|"+nextChar)
            output.append(charUnion)
            index = index + 2
          }
        case _ =>
          if (previousChar.isDefined) output.append(s"|$previousChar.get")
          previousChar = Some(currentChar)
          index = index + 1
      }
    }
    output.deleteCharAt(0)
    "(" + output.result + ")"
  }
}
