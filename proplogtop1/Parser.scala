//
// A Proplog parser.
//
// Andrei de A. Formiga, 2009-03-29
//

package proplog

import java.io.Reader
import scala.util.parsing.combinator._

case class ParseError(msg: String) extends Exception


object Parser extends RegexParsers {
  val ident: Parser[String] = """[a-z]([a-zA-Z0-9])*""".r
  val body: Parser[List[String]] = repsep(ident, ",")
  val goal: Parser[String] = ident<~"?"
  val clause: Parser[Clause] = ident~opt(":-"~body)<~"." ^^
     { case head~Some(":-"~bdy) => Clause(head, bdy)
       case head~None => Clause(head, List()) }

  val clauses: Parser[List[Clause]] = rep(clause) // repsep(clause, "\n")

  def parseClause(clauseStr: String) = parseAll(clause, clauseStr) match {
    case Success(r, i) => r
    case Failure(msg, _) => throw ParseError(msg)
    case Error(msg, _) => throw ParseError(msg)
  }

  def parseClauses(reader: Reader) = parseAll(clauses, reader) match {
    case Success(r, i) => r
    case Failure(msg, _) => throw ParseError(msg)
    case Error(msg, _) => throw ParseError(msg)
  }

  def parseGoal(goalStr: String) = parseAll(goal, goalStr) match {
    case Success(r, i) => r
    case Failure(msg, _) => throw ParseError(msg)
    case Error(msg, _) => throw ParseError(msg)
  }
}
