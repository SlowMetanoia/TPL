package HashTable
/*
import scala.collection.mutable

class ChainTable[ K, V ] extends HashTable[ K, V ] {
  case class Record( values: List[ (K, V) ], hash: Int )
  
  implicit
  object recOrd extends Ordering[ Record ] {
    override def compare( x: Record, y: Record ): Int = x.hash - y.hash
  }
  
  var table = mutable.PriorityQueue[ Record ]()
  
  override
  def insert( value: (K, V) ): Unit = {
    if(recordOf(value._1).isDefined) {
    
    }
  }
  
  def recordOf( key: K ): Option[ Record ] =
    table.find(rec => rec.hash == key.hashCode())
  
  override
  def lookUp( key: K ): Option[ V ] =
    recordOf(key)
      .flatMap(
        _.values
         .find { case (k, _) => k == key }
         .map(_._2)
        )
}

object ChainTable {
  def apply[ K, V ]( ): ChainTable[ K, V ] = new ChainTable()
}
*/