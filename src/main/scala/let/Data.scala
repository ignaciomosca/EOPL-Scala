package let

case class Program(value: Expression)

sealed trait ExpressedValue

object ExpressedValue {

  case class IntegerValue(value: Integer) extends ExpressedValue

  case class BooleanValue(value: Boolean) extends ExpressedValue

  case class ExprList(value: List[ExpressedValue]) extends ExpressedValue

}

sealed trait BinOp

object BinOp {

  case object Add extends BinOp

  case object Sub extends BinOp

  case object Mul extends BinOp

  case object Div extends BinOp

  case object Gt extends BinOp

  case object Le extends BinOp

  case object Eq extends BinOp

  case object Cons extends BinOp

}

sealed trait UnaryOp

object UnaryOp {

  case object Car extends UnaryOp

  case object Cdr extends UnaryOp

  case object Minus extends UnaryOp

  case object IsZero extends UnaryOp

}

sealed trait DenotedValue

object DenotedValue {

  case class DenoNum(value: Int) extends DenotedValue

  case class DenoBool(value: Boolean) extends DenotedValue

}

sealed trait Expression

object Expression {

  case class ConstExpr(value: ExpressedValue) extends Expression

  case class VarExpr(value: String) extends Expression

  case class LetExpr(value: List[(String, Expression)]) extends Expression

  case class LetStarExpr(value: List[(String, Expression)]) extends Expression

  case class BinOpExpr(op: BinOp, exp1: Expression, exp2: Expression) extends Expression

  case class UnaryOpExpr(op: UnaryOp, exp: Expression) extends Expression

  case object EmptyListExpr extends Expression

  case class ListExpr(value: List[Expression]) extends Expression

  case class CondExpr(value: List[(Expression, Expression)]) extends Expression

}

object Data {

  type Environment = Map[String, ExpressedValue]

  def empty: Environment = Map.empty

  def initEnvironment: List[(String, ExpressedValue)] => Environment = _.toMap

  def extend(element: String, value: ExpressedValue, environment: Environment): Environment = {
    environment.updated(element, value)
  }

  def extendMany(variables: List[(String, ExpressedValue)], environment: Environment): Environment = {
    variables.foldLeft(empty) { (acc, i) => extend(i._1, i._2, acc) }
  }

  def apply(env: Environment, variable: String): Option[ExpressedValue] = env.get(variable)

  def applyForce(env: Environment, variable: String): ExpressedValue = env(variable)


}