trait Node
case class Empty() extends Node
case class Data(id: Int, name: String) extends Node

object OpenAddressingHash {
  def main(args: Array[String]): Unit = {
    // 命令数の取得
    val n = io.StdIn.readLine.toInt

    // 命令の実行
    val hashMap: Array[Node] = Array.fill(n)(new Empty())
    val results =
      (1 to n).toList
        .foldLeft((hashMap, Nil: List[Option[String]]))((acc, _) => {
          val operation = io.StdIn.readLine
          val data = io.StdIn.readLine
          val (m, r) = executeOperation(acc._1, operation, data)
          (m, r :: acc._2)
        })

    results._2.foreach { println }
  }

  def executeOperation(hashMap: Array[Node],
                       operation: String,
                       data: String): (Array[Node], Option[String]) = {
    operation match {
      case "add"    => operationController(hashMap, data, add)
      // case "delete" => delete(hashMap)(data)
      // case "search" => search(hashMap)(data)
    }
  }

  def operationController(
      hashMap: Array[Node],
      data: String,
      f: (Array[Node], Int, Data) => (Array[Node], Option[String]))
    : (Array[Node], Option[String]) = {

    def set(hashMap: Array[Node],
            hash: Int,
            data: Data): (Array[Node], Option[String]) = {
      hashMap(hash) match {
        case _: Empty => { f(hashMap, hash, data) }
        case _: Data => {
          set(hashMap, computeHash(hash + 1, hashMap.length), data)
        }
      }
    }

    val d = {
      val a = data.split(' ')
      Data(a(0).toInt, a(1))
    }
    set(hashMap, computeHash(d.id, hashMap.length), d)

  }

  def add(hashMap: Array[Node],
          hash: Int,
          data: Data): (Array[Node], Option[String]) = {
    hashMap(hash) = data
    (hashMap, None)
  }

  def delete(hashMap: Array[Node],
             hash: Int,
             data: Data): (Array[Node], Option[String]) = {
    hashMap(hash) = new Empty
    (hashMap, None)
  }

  def search(hashMap: Array[Node])(data: String): Option[String] = {
    Some("search")
  }

  def computeHash(key: Int, max: Int): Int = { key % max }

}
