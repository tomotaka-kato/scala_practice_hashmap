trait Node
case class Empty() extends Node
case class Data(id: Int, name: String) extends Node

object OpenAddressingHash {
  def main(args: Array[String]): Unit = {
    val n = readLine.toInt 
    val results = operation(n)

    results.foreach(x => x.map( println ))

  }

  /**
   * ユーザーから命令を受け取って実際の処理を行う。
   * 処理結果のSeqを返却する。
   */
  def operation(n: Int): Seq[Option[String]] = {
    // ハッシュマップ
    val map: Array[Node] = Array.fill(n)(Empty())

    // 処理内容を受け取って、各処理を行う
    for (_ <- 1 to n) yield {
      val action = Util.readLine
      action match {
        case "add" => {
          val array = Util.readLine.split(" ")
          add(map, Data(array(0).toInt, array(1)))
          None
        }
        case "search" => { 
          val index = search(map, Util.readLine.toInt) 
          if(index == -1) Some( "未登録データ" )
          else Some(map(index).asInstanceOf[Data].name)
        }
        case "delete" => delete(map, Util.readLine.toInt); None
        case _ => None
      }
    }
 }

/**
 * hashへの追加処理
 */
  def add(m: Array[Node], data: Data): Unit = {
    def loop(hash: Int, max: Int): Unit = {
      m(hash) match { 
       case _: Empty => m(hash) = data 
       case _ => loop(( hash + 1 ) % max, max)
      }
    }
    
    loop(data.id % m.length, m.length)
  }

/**
 * データがすでにあればそのインデックスを、
 * なければ-1を返却する。
 */
  def search(m: Array[Node], id: Int): Int = {
    def loop(hash: Int, max: Int): Int = {
      m(hash) match {
        case _: Empty => -1
        case d: Data => {
          if( d.id == id ) return hash
          else loop(( hash + 1 ) % max, max)
        }
      }
    }
    loop(id % m.length, m.length)
  }

/**
 * 指定されたidのデータを削除する。
 */
  def delete(m: Array[Node], id: Int): Unit = {
    val index = search(m, id)
    // データがあった場合にのみ削除を行う。
    if(index != -1) m(index) = Empty()
  }


}




object Util {

  def readLine:String = { 
    io.StdIn.readLine()
  }

  /**
   * 対象のインデックスが空かどうかの判定
   */
  def isEmpty(m: Array[Node], index: Int): Boolean = {
    m(index) match {
      case _: Empty => true
      case _ => false
    }
  }

}
