import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object IdTable extends App{
  
  case class GID(scopeLvl:Int,scopeNumber:Int,id:Int)
  object GID{
    def apply( scope: Scope, id: Int ): GID = new GID(scope.lvl,scope.number, id)
  }
  
  case class Scope( lvl: Int, number: Int )
  
  sealed trait TElem
  
  sealed trait ITElem extends TElem
  
  sealed trait MTElem extends TElem
  
  case class Id( name: String ) extends ITElem with MTElem
  
  sealed
  trait ITable extends TElem {
    val parent: ITable
    val scope: Scope
    val elements: collection.Map[ Int, TElem ]
    
    
    @tailrec
    final
    def getScope( id: String ): Option[GID] =
      elements.find(_._2 == Id(id)) match {
        case Some(value) => Some(GID(scope.lvl,scope.number,value._1))
        case None => if(parent != null) parent.getScope(id) else None
      }
    
    
    private final
    def mkPrintable( lvl: Int = 0 ): String = {
      val shift: String = "\t" * lvl
      ( Seq(scope.toString, "elements:") ++ elements.values.collect {
        case id: Id => id.name
        case table: ITable => table.mkPrintable(lvl + 1)
      } ).map(shift + _).mkString("\n")
    }
    
    
    override final def toString: String = mkPrintable()
  }
  
  case class Table( parent: Table, scope: Scope, elements: HashMap[ Int, ITElem ] ) extends ITable
  
  case
  class MutableTable(
                      parent: MutableTable = null,
                      scope: Scope = Scope(0, 0),
                      elements: collection.mutable.HashMap[ Int, MTElem ] = collection.mutable.HashMap.empty
                    ) extends ITable with MTElem {
    private var currentMaxKey = 0
    private var currentTable: MutableTable = this
    private var scopes: Array[ Int ] = Array(0)
    private var currentScope: Scope = scope
    
    def up( ): Unit = {
      if(currentScope.lvl + 1 == scopes.length) scopes = scopes.appended(0)
      currentScope = Scope(currentScope.lvl + 1, scopes(currentScope.lvl + 1))
      scopes(currentScope.lvl) += 1
      currentTable = MutableTable(currentTable, currentScope)
      elements += currentMaxKey -> currentTable
      currentMaxKey = 0
    }
    
    def down( ): Unit = {
      currentTable = currentTable.parent
      currentScope = currentTable.scope
      currentMaxKey = currentTable.elements.keys.max + 1
    }
    
    def initId( name: String ): GID = {
      elements += currentMaxKey -> Id(name)
      val result = GID(currentScope,currentMaxKey)
      currentMaxKey += 1
      result
    }
    def exclude:MutableTable = {
      MutableTable(
        parent,
        scope,
        elements.collect{
          case (k,Id(id)) if id != "while" => (k,Id(id))
          case (k,MutableTable(p,s,t))  => (k,MutableTable(p,s,t).exclude)
        }
        )
    }
    def getOrInit( name: String ): GID = currentTable.getScope(name).getOrElse(currentTable.initId(name))
  }
  
  def getTable: MutableTable = MutableTable()
  
  val table = getTable
  table.getOrInit("a")
  table.getOrInit("b")
  table.getOrInit("b")
  table.getOrInit("b")
  table.getOrInit("b")
  table.up()
  table.getOrInit("c")
  table.getOrInit("е")
  table.down()
  table.up()
  table.getOrInit("c")
  table.down()
  table.getOrInit("c")
  table.getOrInit("d")
  
  println(table)
  
}