package test

object Main {

  def main(args: Array[String]): Unit = {
    // println(s"1 + 1 == 2 => ${verified.test}")
    check()
  }

  def check() = {
    println(s"Factorial of 10 is: ${verified.factorial(10)}")
  }
}
