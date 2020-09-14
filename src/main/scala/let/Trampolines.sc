import scala.annotation.tailrec

sealed trait Trampoline[+A] {
  @tailrec
  final def run: A = this match {
    case Done(a) => a
    case More(k) => k().run
  }
}
case class Done[+A](a: A) extends Trampoline[A]
case class More[+A](f: () => Trampoline[A]) extends Trampoline[A]

object Ejemplo {
  def par[A](list: List[A]): Boolean = list match {
    case Nil => true
    case _ :: xs=> impar(xs)
  }
  def impar[A](list: List[A]): Boolean = list match {
    case Nil => false
    case _ :: xs=> par(xs)
  }
}

def test: Unit = {
  val esPar = Ejemplo.par(List.fill(1000000000)(0))
  println(s"Es par? $esPar")
}

object EjemploTrampolines {
  def par[A](list: List[A]): Trampoline[Boolean] = list match {
    case Nil => Done(true)
    case _ :: xs=> impar(xs)
  }
  def impar[A](list: List[A]): Trampoline[Boolean] = list match {
    case Nil => Done(false)
    case _ :: xs=> par(xs)
  }
 }

def testTrampoline: Unit = {
  val esPar = EjemploTrampolines.par(List.fill(1000000000)(0)).run
  println(s"Es par? $esPar")
}