package debox.set

import org.scalatest.FunSuite

class SetTest extends FunSuite {
  test("basic operations on BitmaskSet[Double]") {
    val set = new BitmaskSet(Array.ofDim[Double](8), Array(0), 0, 8)

    assert(set.length === 0)
    assert(set(13.0) === false)
    assert(set(22.0) === false)

    assert(set.add(22.0) === true)
    assert(set.length === 1)
    assert(set(13.0) === false)
    assert(set(22.0) === true)

    assert(set.add(22.0) === false)
    assert(set.length === 1)
    assert(set(13.0) === false)
    assert(set(22.0) === true)

    assert(set.remove(13.0) === false)
    assert(set.length === 1)
    assert(set(13.0) === false)
    assert(set(22.0) === true)

    assert(set.remove(22.0) === true)
    assert(set.length === 0)
    assert(set(13.0) === false)
    assert(set(22.0) === false)
  }

  test("BitmaskSet[String] should also work") {
    val set = new BitmaskSet(Array.ofDim[String](8), Array(0), 0, 8)
    val data = "iwejgiewjgtwu7tuhvudsvudshvusy8wqr83ur3hrtewhgijgsji"
    data.map(_.toString).foreach(s => set.add(s))
    assert(set("i") === true)
    assert(set("3") === true)
    assert(set("Z") === false)
    assert(set("Q") === false)
  }

  test("BitmaskSet[Int] should grow") {
    val set = new BitmaskSet(Array.ofDim[Int](8), Array(0), 0, 8)

    for(i <- 1 to 200) {
      assert(set.add(i) === true)
      assert(set(i) === true)
      assert(set.length == i)
    }

    assert(set.toArray === (1 to 200).toArray)

    for(i <- 1 to 200) {
      assert(set.add(i) === false)
      assert(set(i) === true)
      assert(set.length == 200)
    }
  }

  test("BitmaskSet[Long] from array") {
    val data = Array[Long](1957266643912152636L, 4234701304488201248L, 3407448500372609355L, -9085861913294576356L, 7835837418221060084L, 8688831878112777836L, -5344336607624321891L, 8102120120325845174L, -8607959420840404615L, 5660132306761509969L, 4138817121285214238L, 8924914936314637556L, -5008175716861204907L, 6564271600358984880L, 4961506592052227120L, 8966366274789319199L, 3542310634346313570L, -4076356322417740323L, -5241553243300096054L, 2110546620353239863L, -3053617798717226672L, -7787219951152127966L, -3925634365089087756L, -1256709079841322897L, -814924892484336860L, -5307659173564528581L, -2713590573787961011L, -6512105205901856341L, -5324046727381287757L, -7609453620277868071L, -6178693192038098811L, -1060887370717677303L)

    val set1 = Set(data)
    assert(set1.length == 32)

    val set2 = Set.empty[Long]
    data.foreach(n => set2.add(n))
    assert(set2.length == 32)
  }

}
