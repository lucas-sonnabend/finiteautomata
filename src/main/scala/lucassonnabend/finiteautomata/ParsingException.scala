package finiteautomata

/**
  * Exception that is generated when parsing a regex fails
  */
class ParsingException(symbol: Char, index: Int, message: String) extends Exception(s"Error parsing $symbol at index $index: $message") {
}
