import scala.util.parsing.combinator.RegexParsers

object Lab1 extends App{
  val identifierPattern = """[a-zA-Z_][\w]*""".r
  val strConstPattern = """\".*\"""".r
  sealed trait Lexeme
  sealed trait Value extends Lexeme
  case class ConstStr(value:String) extends Value
  case class Identifier(value:String) extends Value
  case class Operator(op:String) extends Lexeme
  case class KeyWord(kw:String) extends Lexeme
  case class Condition(l:Value,op:Operator,r:Value) extends Lexeme
  
  object Lexer extends RegexParsers{
    val delimiter = """\s*""".r
    
    def id:Parser[Identifier]     = delimiter ~> identifierPattern <~ delimiter^^Identifier
    def constStr:Parser[ConstStr] = delimiter ~> strConstPattern <~ delimiter^^ConstStr
    def value:Parser[Value]       = id|constStr
    def operator:Parser[Operator] = ("<="|">="|"=")^^Operator
    def kw:Parser[KeyWord]        = delimiter ~>("do"|"while")<~ delimiter^^KeyWord
    def cond:Parser[Condition]    = "("~>value~operator~value<~")"^^{ case l~op~r => Condition(l,op,r) }
    def any:Parser[List[Lexeme]]  = rep(cond|value|operator|kw)
  }
  val testData = """
                   |a =           "fgsdfg"
                   |b =       "dfgdfjhksdfgsdfg"
                   |c     =  "sg;lksjdfg"
                   |do
                   |
                   |d = "dfg;hlkdfgh"
                   |a = d
                   |
                   |
                   |
                   |do
                   |c = b
                   |
                   |
                   |while("fdgdf">=a)
                   |while(d<=c)
                   |g    = "well,well"
                   |
                   |do
                   |a = c
                   |while(c<=b)
                   |
                   |""".stripMargin
  val test2 = """a=b while (b)"""
  println(Lexer.parse(Lexer.any, test2).get.mkString("\n"))
}
