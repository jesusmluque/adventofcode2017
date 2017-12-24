import scalaz._
import Scalaz._
import scala.annotation.tailrec

@tailrec
def redistribute(v:Vector[Int], index: Int, resto: Int):Vector[Int] = {
  if (resto == 0)
    v
  else {
    val (newVector, newIndex) = try {
      (v.take(index) ++ Vector(v(index) + 1) ++ v.takeRight(v.length - index - 1), index + 1)
    } catch {
      case _: Exception => (Vector(v(0) + 1) ++ v.takeRight(v.length - 1), 1)
    }
    redistribute(newVector, newIndex, resto - 1)
  }
}
@tailrec
def cycle(memo: List[Int], banks: Vector[Int], steps: Int):Int = {
  if (memo.contains(banks.hashCode()) || steps > 10000)
    steps
  else {
    val max = banks.maximum.get
    val maxIdx = banks.indexOf(max)

    val newVector = redistribute(banks.take(maxIdx) ++ Vector(0) ++ banks.takeRight(banks.length - maxIdx - 1), maxIdx + 1, max)

    cycle(banks.hashCode() :: memo, newVector, steps + 1)
  }
}

val v = Vector(5,1,10,0,1,7,13,14,3,12,8,10,7,12,0,6)
cycle(List(), v, 0)