package let

sealed trait Expression

object Expression {

  case class ConstExpr(value: Values) extends Expression

  case class VarExpr(value: String) extends Expression

  case class LetExpr(id: String, exp: Expression, body: Expression) extends Expression

  case class DiffExpr(exp1: Expression, exp2: Expression) extends Expression

  case class ZeroExpr(exp: Expression) extends Expression

  case class CondExpr(ifExp: Expression, thenExp: Expression, elseExp: Expression) extends Expression

  case class ProcExpr(name: String, exp: Expression) extends Expression

  case class CallExpr(exp1: Expression, exp2: Expression) extends Expression

}
