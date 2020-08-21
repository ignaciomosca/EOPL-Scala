package let

import let.Values.ProcedureValue

case class Environment(bindings: Map[String, Int])

object Environment {

  def empty: Environment = Environment(Map.empty)

  def extend(
              element: String,
              ref: Int,
              environment: Environment
  ): Environment = {
    val updatedEnvironment = environment.bindings.updated(element, ref)
    Environment(updatedEnvironment)
  }

  def extendRec(recProcs: List[(String, String, Expression)], env: Environment, store: Store[Values]) : (Environment, Store[Values]) = recProcs match {
    case Nil => (env, store)
    case (name,param,body)::next =>
      val procedureValue = ProcedureValue(Procedure(param, body, env))
      val (ref, updatedStore) = store.newRef(procedureValue)
      val ext = extend(name, ref, env)
      extendRec(next, ext, updatedStore)
  }

  def apply(env: Environment, variable: String): Int =
    env.bindings.get(variable) match {
      case Some(value) => value
      case None => throw new Exception(s"variable $variable not found")
    }

}
