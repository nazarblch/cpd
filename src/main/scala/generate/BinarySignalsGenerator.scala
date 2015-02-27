package generate

import java.util.Random


object BinarySignalsGenerator {

  val r = new Random()

  def nextBoolean(p: Double): Boolean = {
    r.nextDouble() < p
  }

  def getDirection: Int = {
    if(r.nextBoolean()) 1 else -1
  }

  def draw(size: Int, averageInterval: Int, kant: Int = 0): Array[Boolean] = {
    val p = 1.0 / averageInterval
    var count: Int  = (size * p + getDirection * math.sqrt(size * p * (1 - p)) / 2).toInt

    assert(count >= 0)

    val res = Array.fill[Boolean](size)(false)

    (1 to count).foreach(c => res(r.nextInt(size)) = true)

    Array.fill[Boolean](kant)(false) ++ res ++ Array.fill[Boolean](kant)(false)
  }

}
