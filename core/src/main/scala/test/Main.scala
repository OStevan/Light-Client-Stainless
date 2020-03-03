package test

object Main {

  def main(args: Array[String]): Unit = {
    println(s"max(2, 3) -> ${TestMax.max(BigInt(2), BigInt(3))}")
  }
}
