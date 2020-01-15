import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap

class Program(val body : List[Statement])
class Scope(val name : String, val parent : Scope) {
  var vars : HashMap[String, Expr] = new HashMap[String, Expr]
  var funcs : HashMap[String, Function] = new HashMap[String, Function]
}

trait Statement

case class PrintStatement(val msg : Expr) extends Statement
case class ReturnStatement(val ret : Expr) extends Statement
case class IfStatement(val cond : Condition, val tr : List[Statement], val fl : List[Statement]) extends Statement
case class Function(val name : String, val args : List[String], val body : List[Statement]) extends Statement {
  var ret : Expr = null
}
case class VariableDeclaration(name : String, val value : Expr) extends Statement
class Expr extends Statement

case class Ident(name : String) extends Expr
case class FunctionCall(val name : String, val args : List[Expr]) extends Expr
case class Atom(value : String) extends Expr
case class Number(value : Int) extends Expr
case class Condition(val op : String, val a : Expr, val b : Expr) extends Expr
case class Bool(val value : Boolean) extends Expr
case class Op(val op : String, val a : Expr, val b : Expr) extends Expr

class RecLangParser extends JavaTokenParsers{

  def program : Parser[Program] = codeblock ^^ {
    s => new Program(s)
  }

  def codeblock : Parser[List[Statement]] = repsep(statement, ";")

  def statement : Parser[Statement] = function | variableDeclaration | printStatement

  def function : Parser[Function] = ("let" ~> ident) ~ opt("with" ~> args) ~ "=" ~ ("{" ~> funcblock <~ "}") ^^ {
    case name ~ args ~ "=" ~ body => {
      new Function(name, args.getOrElse(List()), body)
    }
  }

  def variableDeclaration : Parser[VariableDeclaration] = ("let" ~> ident) ~ ("=" ~> expr) ^^ {
    case name ~ value => {
      new VariableDeclaration(name, value)
    }
  }

  def args = rep(ident)

  def funcblock : Parser[List[Statement]] = repsep(funcstat, ";")

  def funcstat : Parser[Statement] = statement | ifStatement | returnStatement

  def printStatement : Parser[PrintStatement] = ("out" ~> expr) ^^ {
    case msg => {
      new PrintStatement(msg)
    }
  }

  def ifStatement : Parser[IfStatement] = ("if" ~> cond <~ "{") ~ funcblock ~ ("}" ~> "else" ~> "{" ~> funcblock <~ "}" ) ^^ {
    case cond ~ ifblock ~ elseblock => {
      new IfStatement(cond, ifblock, elseblock)
    }
  }

  def cond : Parser[Condition] = term ~ opt(("=="|"<>"|">"|"<"|">="|"<=") ~ term) ^^ {
    case a ~ None => Condition("NOP", a, Bool(false))
    case a ~ Some(op ~ b) => Condition(op, a, b)
  }

  def returnStatement : Parser[ReturnStatement] = ("return" ~> expr) ^^ {
    case ret => {
      new ReturnStatement(ret)
    }
  }

  def funcCall : Parser[FunctionCall] = ident ~ (callargs) ^^ {
    case name ~ args => {
      new FunctionCall(name, args)
    }
  }

  def callargs = rep(expr)

  def expr : Parser[Expr] = term ~ opt(("+"|"-"|"*"|"/") ~ term) ^^ {
    case a ~ None => a
    case a ~ Some(op ~ b) => Op(op, a, b)
  }

  def term : Parser[Expr] = ("true" | "false") ^^ {case "true" => new Bool(true) case "false" => new Bool(false)} | funcCall | stringLiteral ^^ {new Atom(_)} | wholeNumber ^^ {case a => new Number(a.toInt)} | ident ^^ { new Ident(_)}

}
