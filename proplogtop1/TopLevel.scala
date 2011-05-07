
//
// TopLevel.scala
// A top-level CLI interface for the Proplog interpreter
//
// Andrei de A. Formiga, 2011-05-04
//

package proplog

object TopLevel {
  def parseFile(name: String, file: java.io.File): List[Clause] = {
    try {
      Parser.parseClauses(new java.io.FileReader(file))
    } catch {
      case ParseError(_) => println("Erro de sintaxe no arquivo " + name); exit(0)
    }
  }

  def main(args: Array[String]) {
    println("Interpretador Proplog, versão 1.0")
    println("Andrei de Araújo Formiga, 2010-2011")
    println("")

    var bdname = ""
    if (args.length < 1) {
      print("Banco de dados: ")
      bdname = readLine()
    } 
    else bdname = args(0)

    val file = new java.io.File(bdname)
    if (!file.exists()) {
      println("Arquivo " + bdname + " não existe")
      exit(0)
    }

    println("Importando banco de dados do arquivo " + bdname + "...")

    val clauses = parseFile(bdname, file)
    println("Banco de dados importado com sucesso")
    println("")
    
    println("Digite uma consulta (nome de proposição seguido de ?) ou 'sair'")

    var done = false
    var line = ""
    while (!done) {
      print("> ")
      line = readLine()
      if (line == "sair" || line == "sair.")
        done = true
      else {
        try {
          val goal = Parser.parseGoal(line)
          if (Interpreter.establishGoal(clauses, goal))
            println("Sim.")
          else
            println("Não.")
        } catch {
          case ParseError(_) => println("Consulta ou comando não reconhecido.")
        }
        
      }
    }
        
  } 
}
