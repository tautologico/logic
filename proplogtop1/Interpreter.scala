
// 
// A Top-down interpreter for Proplog
// Based on the interpreter from the book
// "Computing with Logic" by David Maier and David S. Warren
//
// Andrei de A. Formiga, 2009-05-05
//

package proplog

case class Clause(head: String, body: List[String])

object Interpreter {
  def establish(clauses: List[Clause], goals: List[String]) : Boolean = 
    goals match {
      case Nil => true
      case _ => goals.forall(establishGoal(clauses, _))
    }

  def establishGoal(clauses: List[Clause], goal: String) : Boolean = {
    val matchingClauses = clauses.filter(_.head == goal)
    matchingClauses.exists(c => establish(clauses, c.body))
  }

}
