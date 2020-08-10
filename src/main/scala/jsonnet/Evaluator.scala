package jsonnet

object Evaluator {
  def evaluate(expr: Expr, scope: Map[String, Value]): Value = expr match {
    case Expr.Str(s) => Value.Str(s)
    case Expr.Num(i) => Value.Num(i)
    case Expr.Bool(b) => Value.Bool(b)
    case Expr.Dict(kvs) => Value.Dict(kvs.map { case (k, v) => (k, evaluate(v, scope)) })
    case Expr.Plus(left, right) =>
      (evaluate(left, scope), evaluate(right, scope)) match {
        case (Value.Str(leftStr), Value.Str(rightStr)) => Value.Str(leftStr + rightStr)
        case (Value.Num(leftNum), Value.Num(rightNum)) => Value.Num(leftNum + rightNum)
      }
    case Expr.Local(name, assigned, body) =>
      val assignedValue = evaluate(assigned, scope)
      evaluate(body, scope + (name -> assignedValue))
    case Expr.Ident(name) => scope(name)
    case Expr.Call(expr, args) =>
      val Value.Func(call) = evaluate(expr, scope)
      val evaluatedArgs = args.map(evaluate(_, scope))
      call(evaluatedArgs)
    case Expr.Func(argNames, body) =>
      Value.Func(args => evaluate(body, scope ++ argNames.zip(args)))
  }

  def serialize(v: Value): String = v match {
    case Value.Str(s) => "\"" + s + "\""
    case Value.Num(i) => i.toString
    case Value.Bool(i) => i.toString
    case Value.Dict(kvs) =>
      kvs.map { case (k, v) => "\"" + k + "\": " + serialize(v) }.mkString("{", ", ", "}")
  }

  def jsonnet(input: String): String = {
    serialize(evaluate(fastparse.parse(input, Parser.expr(_)).get.value, Map.empty))
  }
}
