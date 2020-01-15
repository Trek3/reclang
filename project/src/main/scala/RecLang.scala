object RecLang {

  def main(args : Array[String]) {
    val p = new RecLangParser

    args.foreach{
      filename => {
        val src = scala.io.Source.fromFile(filename)

        val lines = src.mkString

        p.parseAll(p.program, lines) match {
          case p.Success(r, _) => {
            new Interpreter(r).run
          }
          case x => println(x)
        }
      }
    }
  }
}
