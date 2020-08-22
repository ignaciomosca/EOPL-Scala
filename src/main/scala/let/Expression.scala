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

  case class LetrecExpr(recProcs: List[RecProc], letrecBody: Expression) extends Expression

  case class BeginExpr(expressions: List[Expression]) extends Expression

  case class AssignExpr(name: String, exp: Expression) extends Expression

  case class NewPairExpr(left: Expression, right: Expression) extends Expression

  case class GetRightPairExpr(exp: Expression) extends Expression

  case class GetLeftPairExpr(exp: Expression) extends Expression

  case class SetRightPairExpr(left: Expression, right: Expression) extends Expression

  case class SetLeftPairExpr(left: Expression, right: Expression) extends Expression


}
