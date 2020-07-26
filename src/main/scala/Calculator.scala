object Calculator {
  def isValidInput(input: String): Boolean = 
    hasMatchingBrackets(input) && 
    hasCorrectlyClosedBrackets(input) &&
    input.matches("[0-9 \\+ \\- \\( \\) \\/ \\* \\s]*")

  def calculate(input: String): Double = {
    if (input.matches(".*[\\(\\)]+.*")){
      val (left, right) = getBracketsIndexTuples(input).head
      return calculate(input.substring(0, left) + calculate(input.substring(left + 1, right)) + input.substring(right + 1))
    }

    val numbers: Array[Double] = "\\d+(\\.\\d+)*".r.findAllIn(input).toArray.map((s) => s.toDouble)
    val operations: Array[String] = "[\\+\\-\\*\\/]+".r.findAllIn(input).toArray

    val (numbersFinal, _) = squashOperations(numbers, operations)

    return numbersFinal(0)
  }

  private def squashOperations(numbersList: Array[Double], operationsList: Array[String]): (Array[Double], Array[String]) = {
    var numbers = numbersList
    var operations = operationsList
 
    var operationsOmitted = 0
    for(i <- 0 to operations.length - 1){
      operations(operationsOmitted) match {
        case "*" => {
          operations = operations.patch(operationsOmitted, Nil, 1)
          numbers = numbers.updated(operationsOmitted, numbers(operationsOmitted) * numbers(operationsOmitted + 1))
          numbers = numbers.patch(operationsOmitted + 1, Nil, 1)
        }
        case "/" => {
          operations = operations.patch(operationsOmitted, Nil, 1)
          numbers = numbers.updated(operationsOmitted, numbers(operationsOmitted) / numbers(operationsOmitted + 1))
          numbers = numbers.patch(operationsOmitted + 1, Nil, 1)
        }
        case _ => operationsOmitted += 1
      }
    }

    operationsOmitted = 0
    for(i <- 0 to operations.length - 1){
      operations(operationsOmitted) match {
        case "+" => {
          operations = operations.patch(operationsOmitted, Nil, 1)
          numbers = numbers.updated(operationsOmitted, numbers(operationsOmitted) + numbers(operationsOmitted + 1))
          numbers = numbers.patch(operationsOmitted + 1, Nil, 1)
        }
        case "-" => {
          operations = operations.patch(operationsOmitted, Nil, 1)
          numbers = numbers.updated(operationsOmitted, numbers(operationsOmitted) - numbers(operationsOmitted + 1))
          numbers = numbers.patch(operationsOmitted + 1, Nil, 1)
        }
        case _ => operationsOmitted += 1
      }
    }
    return (numbers, operations)
  }

  private def getBracketsIndexTuples(input: String): List[(Int,Int)] = {
    val charList = input.toList
    var leftBrackets = List[(Int, Boolean)]()
    var rightBrackets = List[Int]()

    charList.indices.foreach((i) => charList(i) match {
      case '(' => {
        leftBrackets = leftBrackets :+ (i, false)
        rightBrackets = rightBrackets :+ i
      }
      case ')' => {
        val (left, isMatched) = leftBrackets.filter({
          case (left, isMatched) => !isMatched
        }).last
        rightBrackets = rightBrackets.map({
          case(leftIndex) => if(leftIndex == left) i else leftIndex
        })
        leftBrackets = leftBrackets.map({
          case (l, matched) => if(l == left) (l, true) else (l, matched)
        })
      }
      case _ => 
    })

    return leftBrackets.map({
      case (left, isMatched) => left
    }) zip rightBrackets
  }

  private def hasCorrectlyClosedBrackets(input: String) : Boolean = {
    var openedLeftBrackets = 0 
    input.toList.foreach((character) => character match {
      case '(' => openedLeftBrackets += 1
      case ')' => if(openedLeftBrackets <= 0) return false else openedLeftBrackets -= 1
      case _ => 
    })
    return openedLeftBrackets == 0
  }

  private def hasMatchingBrackets(input: String): Boolean = 
    getNumberOfChars(input, '(') == getNumberOfChars(input, ')')

  private def getNumberOfChars(input: String, character: Char): Int = 
    input.toList.foldLeft(0){(acc,c) => if(c == character) acc + 1 else acc}
}