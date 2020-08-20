package let

case class Store[T](locations: List[T]){
  def newRef(value: T): (Ref, Store[T]) = (locations.size, Store(locations.appended(value)))
  def deRef(ref: Ref): T = locations(ref)
  def setRef(ref: Ref, value: T): Store[T] = Store(locations.updated(ref,value))
}