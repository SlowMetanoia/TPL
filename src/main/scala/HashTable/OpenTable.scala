package HashTable

import scala.annotation.tailrec

class OpenTable[K,V](var table:Array[(K,V)]) extends HashTable[K,V] {
  private val fillingC = 0.5
  private var filled = table.count(_==null)
  
  private def extend():Unit = {
    val newTable = OpenTable(new Array[(K,V)](table.size * 2+1))
    table.filter(_!=null).foreach(newTable.insert)
    table = newTable.table
  }
  
  final override def insert( record: (K, V) ): Unit = {
    var ff = false
    val initHash:Int = record._1.hashCode()
    @tailrec
    def put( record: (K, V), hash:Int ):Unit = {
      if(((initHash == hash)&&ff)||(filled+2)/fillingC>=table.size) {
        extend()
        insert(record)
      } else {
        ff = true
        val lea = if(hash>0) hash % table.length else table.length-1 + hash % table.length
        if ( (table(lea) == null) || (table(lea)._1==record._1) )
        {
          table(hash % table.length) = record
        } else {
          val rehash = hash + lea / 2
          put(record, rehash)
        }
      }
    }
    put(record,initHash)
  }
  override def lookUp( key: K ): Option[ V ] = {
    var ff = false
    val initHash = key.hashCode()
    @tailrec
    def has( hash:Int):Option[V] = {
      if((hash == initHash)&&ff) {
        None
      }else{
        ff = true
        val lea = if(hash>0) hash % table.length else table.length-1 + hash % table.length
        if(table(lea)._1 == key)
          Some(table(hash % table.length)._2)
        else {
          val rehash = hash + lea / 2
          has(rehash)
        }
      }
    }
    has(key.hashCode())
  }
  
  override def toString: String = table.mkString("Array(\n", "\n ", "\n)")
  
}
object OpenTable{
  def apply[K,V](
                  table: Array[(K,V)] = Array[(K,V)]()
                ): OpenTable[K,V] = new OpenTable(table)
}
