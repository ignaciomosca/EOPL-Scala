package let

sealed trait BinOp

object BinOp {

  case object Add extends BinOp

  case object Sub extends BinOp

  case object Mul extends BinOp

  case object Div extends BinOp

  case object Gt extends BinOp

  case object Lt extends BinOp

  case object Eq extends BinOp

}