package linkedData

import org.parboiled2._

class NTriples(val input: ParserInput) extends Parser {

  case class Triple(subj: String, pred: String, obj: String)

  def line = rule {
    capture(zeroOrMore(CharPredicate.Printable)) ~> 
      (x => Triple(x,x,x))
  }

  def comment = ???
  def triple = ???
  def subject = ???
  def predicate = ???
  def obj = ???
  def uriref = ???
  def namedNode = ???
  def literal = ???
  def absoluteUri = ???
}
