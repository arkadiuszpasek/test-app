object Calculator {
  def isValidInput(input: String): Boolean = 
    hasCorrectlyClosedBrackets(input) &&
    input.matches("[0-9 \\d+.\\d+ \\+ \\- \\( \\) \\/ \\* \\s]*")

  def calculate(input: String): Option[Double] = {
    try{
      if(!isValidInput(input)) return None

      // Keep reducing brackets recursively
      if (input.matches(".*[\\(\\)]+.*")){
        val (left, right) = getBracketsIndexTuples(input).head
        calculate(input.substring(left + 1 ,right)) match {
          case Some(result) => return calculate(input.substring(0, left) + result.toString + input.substring(right + 1))
          case None => return None
        }
      }

      // Match a number, or a number with '-' but only if '-' is after another expression or beginning
      // '-' between 2 numbers means subtraction
      val numbers: Array[Double] = "(^|[\\+\\-\\*\\/\\s]\\-)*\\d+(\\.\\d+)*".r.findAllIn(input).toArray.map((s) => {
        s.replaceAll("[\\+\\/\\*]", "").toDouble
      })
      val operations: Array[String] = "[\\+\\-\\*\\/]+".r.findAllIn(input).toArray.map((o) => {
        if (o.length > 1 && o.charAt(1) == '-') o.charAt(0).toString else o
      })

      val (numbersFinal, _) = getsquashedParams(numbers, operations)

      return Some(numbersFinal(0))
    } catch {
      case e: Exception => None
    }
  }

  private def getsquashedParams(numbersList: Array[Double], operationsList: Array[String]): (Array[Double], Array[String]) = {
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
          numbers = numbers.updated(0, numbers(0) + numbers(0 + 1))
          numbers = numbers.patch(0 + 1, Nil, 1)
        }
        case "-" => {
          operations = operations.patch(0, Nil, 1)
          numbers = numbers.updated(0, numbers(0) - numbers(0 + 1))
          numbers = numbers.patch(0 + 1, Nil, 1)
        }
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
}