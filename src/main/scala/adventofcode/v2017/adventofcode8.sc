val input = List(0,1,2,3,4)
val lengths = List(3,4,1,5)
def applyLengths(list: List[Int], long: Int, cursor: Int, skip: Int) = {
  val left = list.take(cursor)
  val right = list.takeRight(list.length - cursor)
  if (right.length >= long)
    (left ++ right.take(long).reverse ++ right.takeRight(right.length - long), (cursor + long + skip) % list.length, skip + 1)
  else
    (right.take(long).reverse ++ left.take(long - right.length).reverse ++ left.takeRight(left.length - long + right.length), (cursor + long + skip) % list.length, skip + 1)
}
def encrypt(list: List[Int], lengths: List[Int]): List[Int] = {

  def doEncrypt(list: List[Int], lengths: List[Int], cursor: Int, skip: Int): List[Int] = {
    lengths match {
      case List() => list
      case a :: rest => {
        val res = applyLengths(list, a, cursor, skip)
        doEncrypt(res._1,rest, res._2, res._2)
      }
    }
  }
  doEncrypt(list, lengths, 0, 0)
}
encrypt(input, lengths)
