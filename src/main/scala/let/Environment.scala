package let

object Environment {

  type Environment = Map[String, Values]

  def empty: Environment = Map.empty

  def initEnvironment: List[(String, Values)] => Environment = _.toMap

  def extend(element: String, value: Values, environment: Environment): Environment = {
    environment.updated(element, value)
  }

  def extendMany(variables: List[(String, Values)], environment: Environment): Environment = {
    variables.foldLeft(empty) { (acc, i) => extend(i._1, i._2, acc) }
  }

  def apply(env: Environment, variable: String): Option[Values] = env.get(variable)

  def applyForce(env: Environment, variable: String): Values = env(variable)


}