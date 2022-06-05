package HashTable

trait HashTable[K,V]{
  def insert(value:(K,V)):Unit
  def lookUp(value:K):Option[V]
}
