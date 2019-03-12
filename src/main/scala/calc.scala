
import cats.Id
import cats.arrow.FunctionK
import cats.free._

import scala.io.StdIn

sealed trait Ops[A]
case class ReadLn[A](f: String => A) extends Ops[A]
case class Add[A](a: A, b: A, f: (A, A) => A) extends Ops[A]
case class Mul[A](a: A, b: A, f: (A, A) => A) extends Ops[A]

object calc {

  type FreeOp[A] = Free[Ops, A]

  def read[A](f: String => A) : FreeOp[A] = Free.liftF(ReadLn(f))

  def add[A](a: A, b: A)(f: (A, A) => A) : FreeOp[A] = Free.liftF(Add(a, b, f))

  def mul[A](a: A, b: A)(f: (A, A) => A) : FreeOp[A] = Free.liftF(Mul(a, b, f))

  def calculate[A](o: FreeOp[A]) : A = {
    val value: FunctionK[Ops, Id] = new FunctionK[Ops, Id] {
      override def apply[B](fa: Ops[B]): Id[B] = fa match {
        case ReadLn(f) => f(StdIn.readLine())
        case Add(x, y, f) => f(x, y)
        case Mul(x, y, f) => f(x, y)
      }
    }
    Free.foldMap(value).apply(o)
  }
}