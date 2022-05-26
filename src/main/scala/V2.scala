object V2 extends App{
  import scala.util.parsing.combinator.RegexParsers
  
  //паттерны.
  val identifierPattern = """[a-zA-Z_][\w]*""".r
  val strConstPattern = """\".*\"""".r
  
  /**
   * Первая лаба по методам трансляции
   * Задание
   * Реализовать программу, выполняющая лексический анализ входного языка и создание
   * таблицы лексем с указанием их типов.
   * Программа должна выдавать сообщения о наличии во входном тексте ошибок, которые могут
   * быть обнаружены на этапе лексического анализа
   *
   * Вариант 17:
   * Входной язык содержит операторы цикла «do ... while (...)», разделённые символом
   * «;». Операторы условия содержат идентификаторы, знаки сравнения «<=», «=>», «=»,
   * строковые константы (последовательность символов в двойных кавычках), знак
   * присваивания «=».
   */
  
  //Это убирает комменты
  def deComment(strings: Iterable[String]): Iterable[String] =
    strings.flatMap(str => str.split("""//(.|s)*"""))
  
  /**
   * Лексер должен убрать лишнее, но не изменять структуру текста.
   * то есть, мы убираем лишние пробелы:
   * "a    = b" =>"a=b"
   * "a b" => "ab"//тут всё равно, что мы клеим имена, главное - задетектить ошибку.
   * "do
   * a= b
   * c=d
   * while       (       a <=     b )" =>
   * "do
   * a=b
   * c=d
   * while(a<=b)"
   */
  object Lexer extends RegexParsers{
    val delimiter = """\s*""".r
    
    def id:Parser[String] = delimiter ~> identifierPattern <~ delimiter
    def constStr:Parser[String] = delimiter ~> strConstPattern <~ delimiter
    def value:Parser[String] = id|constStr
    
    
    def operator:Parser[String] = "<="|">="|"="
    def operation:Parser[String] = value ~ operator ~ value ^^{case v1~op~v2 => v1+op+v2}
    
    def cond:Parser[String] = "("~operation~")"^^{case _~op~_ => "("+op+")"}
    
    def doo:Parser[String] = delimiter ~> "do" <~ delimiter
    def whl:Parser[String] = delimiter ~> "while" ~ delimiter ~ cond ^^{case whl~_~cond => whl + cond}
    def line:Parser[String] = (operation|doo|whl|"\n")^^{
      case "\n" => ""
      case str => str
    }
    def analyze:Parser[List[String]] = rep(line)
  }
  def lexAnalyze(string: String)/*: List[String]*/ =
    Lexer.parseAll(Lexer.analyze,string)
  
  val errTest =
    """
      |a =           "fgsdfg"
      |b =       "dfgdfjhksdfgsdfg"
      |c     =  "sg;lksjdfg"
      |
      |a c
      |""".stripMargin
  
  def laOut(str:String) =
    lexAnalyze(deComment(str.split("\n")).mkString("\n"))
  
  sealed trait Value
  case class Identifier(name:String,id:Int) extends Value
  case class ConstString(value:String) extends Value
  
  sealed trait Structure
  case class Operation(left:Value,op:String,right: Value) extends Structure
  case class While(body:List[Structure], condition:Operation) extends Structure
  
  case class ParsingResult(code:List[Structure],table:Map[String,Int])
  
  class generalParser extends RegexParsers{
    var idTable:Map[String,Int] = Map("$def"->0)
    def getId(name:String):Int =
      idTable.getOrElse(
        name,
        {
          idTable = idTable + ( name -> (idTable.values.max + 1))
          getId(name)
        }
        )
    def hasVar(name:String):Boolean = idTable.contains(name)
    
    //value
    def id:Parser[Identifier] = identifierPattern^^ { name=>
      Identifier(name, getId(name)) }
    def constString:Parser[ConstString] = strConstPattern^^ConstString
    def value:Parser[Value] = id|constString
    
    //operation
    def operator:Parser[String] = "="|">="|"<="
    def operation:Parser[Operation] = value ~ operator ~ value^^{
      case v1~op~v2 => Operation(v1,op,v2)
    }
    
    //do...while(...)
    def DO:Parser[Unit] = "do"^^ { _=>() }
    def WHILE:Parser[Operation] = """while\(""".r~>operation<~"""\)""".r
    def codeLines:Parser[List[Structure]] = rep(cycle | operation)
    def cycle:Parser[While] = DO ~> codeLines ~ WHILE ^^{case body ~ cond => While(body, cond)}
  }
  
  def resultPrint(parsingResult: ParsingResult):Unit = {
    def printLevel(lvl:Int) = print("->"*lvl)
    def printStructure(structure: Structure, lvl:Int = 0):Unit =structure match {
      case While(structures,condition) =>
        printLevel(lvl)
        println("do")
        structures.foreach(struct=>printStructure(struct,lvl+1))
        printLevel(lvl)
        println(s"while $condition")
      case op:Operation=>
        printLevel(lvl)
        println(op)
    }
    def printTable(table:Map[String,Int]) = println(table.mkString("\n"))
    
    println("Execution tree:")
    parsingResult.code.foreach(struct=>printStructure(struct))
    println
    println("Var table:")
    printTable(parsingResult.table-"$def"-"while")
  }
  
  object generalParser{
    def tryToParse( str:String) = {
      val parser = new generalParser()
      val lOut = laOut(str)
      if(lOut.successful){
        val prsR = parser.parseAll(
          parser.codeLines,
          lOut.get.mkString("\n")
          )
        if(prsR.successful)
          resultPrint(ParsingResult(prsR.get,parser.idTable))
        else {
          println("error on execution tree building")
          println(prsR)
        }
      } else {
        println("error in semi-lexical analyzer:")
        println(lOut)
      }
    }
  }
  
  val generalTest ="""
      |a =           "fgsdfg"
      |b =       "dfgdfjhksdfgsdfg"
      |c     =  "sg;lksjdfg"
      |do
      |
      |d = "dfg;hlkdfgh"
      |a = d
      |
      |
      |//sdflgjsdfg
      |do //dfgdglkldfg
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
  val test ="do do while a=a while(a<=a) while(a>=a)"
  generalParser.tryToParse(generalTest)
  generalParser.tryToParse(errTest)
  generalParser.tryToParse(test)
  
  /**
   * Отчёт: как устроено
   * дерево
   * "как работает parsers"
   *
   */
}
