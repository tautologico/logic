
// 
// A Bottom-up interpreter for Proplog
// Based on the interpreter from the book
// "Computing with Logic" by David Maier and David S. Warren
//
// Andrei de A. Formiga, 2009-03-24
//

package proplog

case class Clause(head: String, body: List[String])

object Interpreter {
  def findFacts(clauses: List[Clause], facts: Set[String]) = {
    // the set of new facts that can be inferred
    val newfs = clauses.filter(c => c.body.forall(facts.contains(_))).map(_.head)
    newfs.filter(!facts.contains(_))   // select the facts not already known
  }

  def findAllFacts(clauses: List[Clause], facts: Set[String]): Set[String] = 
    findFacts(clauses, facts) match {
      case Nil   => facts
      case newfs => findAllFacts(clauses, facts ++ newfs)
    }
    
  def establish(clauses: List[Clause], goals: List[String]) = {
    val facts = findAllFacts(clauses, Set.empty)
    goals.forall(facts.contains(_))    
  }

  def printUsage() {
    println("Parameters: List of clauses and goal")
  }

  def interpretFile(fileName: String, goal: String) {
    val clauses = Parser.parseClauses(new java.io.FileReader(fileName))
    if (establish(clauses, List(goal)))
      println("*** This goal is proved ***")
    else
      println("This goal is NOT proved")
  }

  def main(args: Array[String]) {
    if (args.length >= 2)
      interpretFile(args(0), args(1))
    else
      printUsage()
  }
}
