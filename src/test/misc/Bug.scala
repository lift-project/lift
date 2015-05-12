package misc

import org.junit.Test

import scala.collection.immutable.{HashMap, HashSet}

class Bug {
  @Test def bug2() {
    val v0 = new V("v0")
    val v1 = new V("v0")
    val set = new HashSet[V]() + v0
    val map = new HashMap[V,Int]() + (v0->1)
    //v0.id = "new"

    println(v0 == v1)
    println(v0.equals(v1))

    val keys = map.keySet
    println (set -- keys)
    println (set.filterNot(v => keys.contains(v)))
  }
}
