package let

sealed trait Expression

object Expression {

  case class ConstExpr(value: Values) extends Expression

  case class VarExpr(value: String) extends Expression

  case class LetExpr(name: String, exp: Expression, body: Expression) extends Expression

  case class BinOpExpr(op: BinOp, exp1: Expression, exp2: Expression) extends Expression

  case object EmptyListExpr extends Expression

  case class ListExpr(value: List[Expression]) extends Expression

  case class CondExpr(ifExp: Expression, thenExp: Expression, elseExp: Expression) extends Expression

  case class Call(expr: Expression, args: Seq[Expression]) extends Expression

  case class Plus(left: Expression, right:Expression) extends Expression

}
