

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

object V3 extends App{
  sealed trait Lexeme
  
  case class kw(name:String) extends Lexeme
  case class id(name:String) extends Lexeme
  case class const(value:String) extends Lexeme
  case class dlm(value:String) extends Lexeme
  case class opr(value:String) extends Lexeme
  object Lexer{
    val identifierPattern = """[a-zA-Z_][\w]*"""
    val strConstPattern = """\".*\""""
    val multiLineComment = """/\*.*\*/"""
    val patterns = List("do","while","(",")",identifierPattern,strConstPattern,"<=",">=","=").map(_.r)
    
    def removeComments:String => String =  _.split(multiLineComment)
                                          .mkString.split("\n")
                                          .map(_.split("//.*").mkString)
                                          .mkString(" ")
    
    def split:String=>Seq[String] = removeComments.andThen(_.split("""(\s+)"""))
    def getLexemes:Seq[String]=>Seq[Lexeme] = _.collect{
      case "" => None
      case str:String if patterns.exists(r=>r.matches(str)) => Some(str)
      case _ => throw new Exception("unexpected symbol")
    }.collect{
      case Some("do") =>kw("do")
      case Some("while") =>kw("while")
      case Some("(") => dlm("(")
      case Some(")") => dlm(")")
      case Some("<=") => opr("<=")
      case Some(">=") => opr(">=")
      case Some("=") => opr("=")
      case Some(str) if identifierPattern.matches(str) => id(str)
      case Some(str) if strConstPattern.matches(str) => id(str)
    }
    def analyze:String => Seq[Lexeme] =
      removeComments andThen
      split andThen
      getLexemes
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
  Lexer.analyze(testData)
}
