import org.scalatest.funsuite.AnyFunSuite

class CalculatorTest extends AnyFunSuite {
  test("Calculator validates invalid characters"){
    assert(Calculator.isValidInput("2?5") === false)
    assert(Calculator.isValidInput("2+5d") === false)
    assert(Calculator.isValidInput("2+5!") === false)
    assert(Calculator.isValidInput("(2+5)") === true)
    assert(Calculator.isValidInput("[2+5]") === false)
  }

  test("Validates unmatched brackets"){
    assert(Calculator.isValidInput("((2+5)") === false)
    assert(Calculator.isValidInput("())2+5(") === false)
    assert(Calculator.isValidInput("(((2+5)))") === true)
    assert(Calculator.isValidInput("(2+5)*(6+3)") === true)
  }

  test("Calculates brackets before others"){
    assert(Calculator.calculate("3*(2+5)*2") == Some(42.0))
    assert(Calculator.calculate("3*2+5*2") == Some(16.0))
  }

  test("Calculates sample expressions correctly"){
    assert(Calculator.calculate("(1-1)*2+3*(1-3+4)+10/2") == Some(11.0))
    assert(Calculator.calculate("1-2") == Some(-1.0))
    assert(Calculator.calculate("1-8+2") == Some(-5.0))
    assert(Calculator.calculate("2+(1-2)*2+3*1-3+4+10/2") == Some(9.0))
    assert(Calculator.calculate("-8") == Some(-8.0))
    assert(Calculator.calculate("-8-1+3") == Some(-6.0))
    assert(Calculator.calculate("-8+10") == Some(2.0))
    assert(Calculator.calculate("-8-(-5)") == Some(-3.0))
    assert(Calculator.calculate("-8 - ( - 5)") == Some(-3.0))
  }
}