object Calculator {
  def isValidInput(input: String): Boolean = 
    hasMatchingBrackets(input) && 
    input.matches("[0-9 \\+ \\- \\( \\) \\/ \\* \\s]*")

  def hasMatchingBrackets(input: String): Boolean = 
    getNumberOfChars(input, '(') == getNumberOfChars(input, ')')

  def getNumberOfChars(input: String, character: Char): Int = 
    input.toList.foldLeft(0){(acc,c) => if(c == character) acc + 1 else acc}
}