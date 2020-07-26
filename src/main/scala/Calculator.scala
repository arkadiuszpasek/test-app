object Calculator {
  def isValidInput(input: String): Boolean = 
    hasCorrectlyClosedBrackets(input) &&
    input.matches("[0-9 \\d+.\\d+ \\+ \\- \\( \\) \\/ \\* \\s]*")

  def calculate(expression: String): Option[Double] = {
    val input = expression.replaceAll("\\s", "")
    try{
      if(!isValidInput(input)) return None

      // Reduce brackets recursively
      if (input.matches(".*[\\(\\)]+.*")){
        val (left, right) = getBracketsIndexTuples(input).head
        calculate(input.substring(left + 1 ,right)) match {
          case Some(result) => return calculate(input.substring(0, left) + result.toString + input.substring(right + 1))
          case None => return None
        }
      }

      // Match a number, or a number with '-' but only if '-' is after another expression or beginning
      // '-' between 2 numbers means subtraction operation
      val numbers: Array[Double] = "((^|\\+|\\-|\\*|\\/)\\-)*\\d+(\\.\\d+)*".r.findAllIn(input).toArray.map((s) => {
        if(s.matches("[\\+\\/\\*\\-]{2}.*")) s.replaceFirst("[\\+\\/\\*\\-]", "").toDouble else s.toDouble
      })

      // Match all operators that aren't at the beginning, '-' at the beginning means negative first number
      val operations: Array[String] = "(?<!^)[\\+\\-\\*\\/]+".r.findAllIn(input).toArray.map((o) => {
        if (o.length > 1 && o.charAt(1) == '-') o.charAt(0).toString else o
      })

      return Some(getSquashedOperationsResult(numbers, operations))
    } catch {
      case e: Exception => None
    }
  }

  private def getSquashedOperationsResult(numbersList: Array[Double], operationsList: Array[String]): Double = {
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

    for(i <- 0 to operations.length - 1){
      operations(0) match {
        case "+" => {
          operations = operations.patch(0, Nil, 1)
          numbers = numbers.updated(0, numbers(0) + numbers(1))
          numbers = numbers.patch(1, Nil, 1)
        }
        case "-" => {
          operations = operations.patch(0, Nil, 1)
          numbers = numbers.updated(0, numbers(0) - numbers(1))
          numbers = numbers.patch(1, Nil, 1)
        }
      }
    }
    return numbers(0)
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
    val openedBrackets = input.toList.foldLeft(0)((acc, c) => c match {
      case '(' => acc + 1
      case ')' => if(acc <= 0) return false else acc - 1
      case _ => acc
    })
    return openedBrackets == 0
  }
}