package let

sealed trait Continuation
case object EndCont extends Continuation
case class ZeroCont(cont: Continuation) extends Continuation
case class LetCont(name: String, exp: Expression, env: Environment, cont: Continuation) extends Continuation
case class CondCont(exp1: Expression, exp2: Expression, env: Environment, cont: Continuation) extends Continuation
case class DiffCont(exp: Expression, env: Environment, cont:Continuation) extends Continuation
case class DiffAuxCont(value: ExpVal, cont: Continuation) extends Continuation
case class CallExprCont(exp: Expression, env: Environment, cont: Continuation) extends Continuation
case class CallExprAuxCont(value: ExpVal, cont: Continuation) extends Continuation
