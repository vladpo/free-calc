
import calc.{FreeOp, add, mul, read}

object Main extends App {

  val prog: FreeOp[(Int, Int)] = for {
    a <- read(_.toInt)
    b <- read(_.toInt)
    c <- add(a, b)(_+_)
    d <- mul(a, b)(_*_)
  } yield (c, d)

  print(calc.calculate(prog))
}
