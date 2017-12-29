val input = List(0,1,2,3,4)
val lengths = List(3,4,1,5)
def applyLengths(list: List[Int], long: Int, cursor: Int, skip: Int) = {
  val left = list.take(cursor)
  val right = list.takeRight(list.length - cursor)
  if (right.length >= long)
    (left ++ right.take(long).reverse ++ right.takeRight(right.length - long), (cursor + long + skip) % list.length, skip + 1)
  else {

    val listReversed = (right ++ left.take(long - right.length)).reverse
    val newLeft = listReversed.takeRight(long - right.length) ++ left.takeRight(left.length - long + right.length)
    val newRight = listReversed.take(right.length)
    (newLeft ++ newRight, (cursor + long + skip) % list.length, skip + 1)
  }

}
def encrypt(list: List[Int], lengths: List[Int]): (List[Int], Int) = {

  def doEncrypt(list: List[Int], lengths: List[Int], cursor: Int, skip: Int): (List[Int], Int) = {
    lengths match {
      case List() => (list, cursor)
      case a :: rest => {
        val res = applyLengths(list, a, cursor, skip)
        doEncrypt(res._1,rest, res._2, res._3)
      }
    }
  }
  doEncrypt(list, lengths, 0, 0)
}
encrypt(input, lengths)._1.take(2).product
val result = encrypt(Range(0,256).toList,List(192,69,168,160,78,1,166,28,0,83,198,2,254,255,41,12))
result._1(0) * result._1(1)
