package let

case class Store[T](locations: List[T]){
  type Ref = Int
  def newRef(value: T): (Ref, Store[T]) = (locations.size, Store(locations.appended(value)))
  def deRef(ref: Ref): T = locations(ref)
  def setRef(ref: Ref, value: T): Store[T] = Store(locations.updated(ref,value)) //TODO puede dar error
}