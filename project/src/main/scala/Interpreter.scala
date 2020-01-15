import scala.collection.breakOut
import scala.collection.mutable.HashMap

class Interpreter(val p : Program){

  var scope : Scope = new Scope("global", null)

  def run = {
    exec(p.body)
  }

  def setVar(key : String, value : Expr) = {
    scope.vars(key) = value
  }

  def setFunc(key : String, value : Function) = {
    scope.funcs(key) = value
  }

  def getVar(ident : Ident) : Expr = {

    val startScope = scope
    while(!scope.vars.contains(ident.name) && scope.name != "global") scope = scope.parent

    if(scope.vars.contains(ident.name)){
      val value = scope.vars(ident.name)

      scope = startScope

      value
    }
    else {
      scope = startScope
      sys.error("variable " + ident.name + " has not been declared in the current scope")
    }
  }

  def getFunc(name : String) : Function = {

    val startScope = scope
    while(!scope.funcs.contains(name) && scope.name != "global") scope = scope.parent

    if(scope.funcs.contains(name)){
      val value = scope.funcs(name)

      scope = startScope

      value
    }
    else {
      scope = startScope
      sys.error("function " + name + " has not been declared in the current scope.")
    }
  }

  def eval(e : Expr) : Expr = {
    e match {
      case Atom(x) => Atom(x)
      case Number(x) => Number(x)
      case Bool(x) => Bool(x)
      case Ident(name) => eval(getVar(e.asInstanceOf[Ident]))
      case Op(op, a, b) => {
        (op, a, b) match {
          case ("+", Atom(x), Atom(y)) => Atom((x : String).dropRight(1) + (y : String).drop(1))
          case ("-", Atom(x), Atom(y)) => Atom(x.filter(c => !y.contains(c)))
          case ("/", Atom(x), Atom(y)) => Atom(x.filter(c => y.contains(c)))
          case ("+", Number(x), Number(y)) => Number(x + y)
          case ("-", Number(x), Number(y)) => Number(x - y)
          case ("*", Number(x), Number(y)) => Number(x * y)
          case ("/", Number(x), Number(y)) => Number(x / y)
          case ("*", Atom(x), Number(y)) => Atom(x * y)
          case _ => eval(Op(op, eval(a), eval(b)))

        }
      }
      case Condition(op, a, b) => {
        (op, a, b) match {
          case ("==", Number(a), Number(b)) => Bool(a == b)
          case ("<>", Number(a), Number(b)) => Bool(a != b)
          case (">", Number(a), Number(b)) => Bool(a > b)
          case ("<", Number(a), Number(b)) => Bool(a < b)
          case (">=", Number(a), Number(b)) => Bool(a >= b)
          case ("<=", Number(a), Number(b)) => Bool(a <= b)
          case ("NOP", a, _) => eval(a)
          case _ => eval(Condition(op, eval(a), eval(b)))
        }
      }
      case FunctionCall(name, args) => {
        //println("searching function call in " + scope.name)
        try{
          val f = getFunc(e.asInstanceOf[FunctionCall].name)

          //print(f.name)
          execFunc(f, args)
          f.ret
        }
        catch {
          case ex : Exception => {
           // println("searching var with " + scope.name + " " + scope.vars)
            val v = getVar(Ident(e.asInstanceOf[FunctionCall].name))

            eval(v)
          }
        }
      }
    }
  }

  def execFunc(f : Function, args : List[Expr]) = {

    scope = new Scope(f.name, scope)

    setFunc(f.name, f)

    //println("executing function " + f.name + " with args " + args)
    scope.vars = (f.args zip (args.map(x => eval(x))))(breakOut) : HashMap[String, Expr]

    //println(f.body)
    exec(f.body)

    //println("exiting function")
  }

  def exec(tree : List[Statement]) : Unit = {

    if(!tree.isEmpty){
      //println("executing: " + tree.head)
      tree.head match {
        case Function(name, args, body) => {
          setFunc(name, tree.head.asInstanceOf[Function])

          exec(tree.tail)
        }
        case VariableDeclaration(name, value) => {
          if(value.isInstanceOf[FunctionCall]) {

            val f = getFunc(value.asInstanceOf[FunctionCall].name)

            execFunc(f, value.asInstanceOf[FunctionCall].args)

            scope.parent.vars(name) = eval(f.ret)
            scope = scope.parent
          }
          else {
            setVar(name, value)
          }

          exec(tree.tail)
        }
        case PrintStatement(msg) => {
          eval(msg) match {
            case Atom(x) => println(x)
            case Number(x) => println(x)
          }

          exec(tree.tail)
        }
        case IfStatement(cond, tr, fl) => {
          eval(cond) match {
            case Bool(x) => {
              if(x)
                exec(tr)
              else
                exec(fl)
            }
          }

          exec(tree.tail)
        }
        case ReturnStatement(ret) => {
          if(scope.name == "global"){
            sys.error("return statement outside function.")
          }
          else {
            val f = scope.parent.funcs(scope.name)

            f.ret = eval(ret)

          }
        }
        case _ => ()
      }
    }
  }
}
