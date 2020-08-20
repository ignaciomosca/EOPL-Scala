package let

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StoreSpec extends AnyFlatSpec with Matchers {
  "Store" should "set an element in the index ref-1 in a list when adding a newref" in {
    val store = Store[Int](List())
    val add1 = store.newRef(1)
    add1 shouldEqual (0, Store[Int](List(1)))
    add1._2.deRef(0) shouldEqual 1
  }

  "Store" should "return an element in the index ref-1 in a list when adding a newref" in {
    val store = Store[Int](List())
    val add1 = store.newRef(1)
    add1 shouldEqual (0, Store[Int](List(1)))
    val update = add1._2.setRef(0,6)
    update shouldEqual Store[Int](List(6))
  }

}
