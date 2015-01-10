package linkedData

import scala.util.{ Try, Success, Failure }
import org.parboiled2._

object NTriples {
  // Main parsed types
  trait Line       // A parsed line; can either be Blank, Comment, or Triple
  trait Subject    // A triple's subject; can either be a uriref or namednode
  trait Predicate  // A triple's predicate; can either be a uriref or namednode
  trait Object     // A triple's object; will always be a uriref or or namednode or literal

  case class UriRef(uri: String) extends Subject with Object with Predicate
  case class NamedNode(name: String) extends Subject with Object with Predicate
  case class Literal(
    value: String,
    lang: Option[String],
    dataType: Option[UriRef]
    ) extends Object

  case class Blank() extends Line
  case class Comment(text: String) extends Line
  case class Triple(subj: Subject, pred: Predicate, obj: Object) extends Line

  def parse(lines: Iterator[String]): Iterator[Try[Line]] = {
    lines map {l => 
      val p = new NTriples(l)
      p.line.run() match {
        case Failure(e: ParseError) => Failure(new Exception(p.formatError(e)))
        case x                      => x
      }
    }    
  }
}

class NTriples(val input: ParserInput) extends Parser with StringBuilding {

  import CharPredicate.{Alpha, AlphaNum, HexDigit, Printable}
  import NTriples._

  // ntripleDoc ::= line*  

  // line ::= ws* (comment | triple) ? eoln  
  def line: Rule1[Line] = rule { blank | nonBlank }
  private def blank: Rule1[Line] = rule { zeroOrMore(ws) ~ EOI ~ push(Blank()) }
  private def nonBlank: Rule1[Line] = rule { (comment | triple) ~ EOI }

  // comment ::= '#' (character - ( cr | lf ) )*  
  private def comment: Rule1[Comment] = rule {
    '#' ~ zeroOrMore(ws) ~ capture(zeroOrMore(Printable)) ~> (Comment(_))
  }

  // triple ::= subject ws+ predicate ws+ object ws* '.' ws*   
  private def triple: Rule1[Triple] = rule { 
    zeroOrMore(ws) ~ subject ~ 
    oneOrMore(ws) ~ predicate ~ 
    oneOrMore(ws)  ~ obj ~ 
    zeroOrMore(ws) ~ '.' ~ 
    zeroOrMore(ws) ~>
      ((s: Subject, p: Predicate, o: Object) => Triple(s, p, o))
  }

  // subject ::= uriref | namedNode   
  private def subject: Rule1[Subject] = rule {
    uriref | namedNode
  }

  // predicate ::= uriref   
  private def predicate: Rule1[Predicate] = rule {
    uriref
  }

  // object ::= uriref | namedNode | literal   
  private def obj: Rule1[Object] = rule {
    uriref | namedNode | literal
  }

  // uriref ::= '<' absoluteURI '>'  
  private def uriref: Rule1[UriRef] = rule {
    '<' ~ capture(zeroOrMore(!'>' ~ ANY)) ~ '>' ~> (UriRef(_))    
  }

  // namedNode ::= '_:' name  
  private def namedNode: Rule1[NamedNode] = rule {
    '_' ~ ':' ~ capture(name) ~> (NamedNode(_))    
  }

  // literal ::= '"' string '"'   
  private def literal: Rule1[Literal] = rule {
    quotedString ~ optional(lang) ~ optional(dataType) ~> 
      ((v:String, l:Option[String], t:Option[UriRef]) => Literal(v,l,t))
  }

  private def quotedString: Rule1[String] = rule {
    '"' ~ clearSB() ~ Characters ~ '"' ~ push(sb.toString)
  }

  private def lang: Rule1[String] = rule {
    '@' ~ capture(oneOrMore(!ws ~ Printable))
  }

  private def dataType: Rule1[UriRef] = rule {
    2.times('^') ~ uriref
  }

  // absoluteURI ::= ( character - ( '<' | '>' | space ) )+   
  private def absoluteUri = ???

  // ws ::= space | tab  
  private def ws = CharPredicate(" \t")

  // name ::= [A-Za-z][A-Za-z0-9]*   
  private def name = rule {
    Alpha ~ zeroOrMore(AlphaNum)
  }

  // ----
  // Taken directly from the Json parser example
  // https://github.com/sirthias/parboiled2/blob/master/examples/src/main/scala/org/parboiled2/examples/JsonParser.scala
  //
  def Characters = rule { zeroOrMore(NormalChar | '\\' ~ EscapedChar) }

  def NormalChar = rule { !QuoteBackslash ~ ANY ~ appendSB() }

  def EscapedChar = rule (
    QuoteSlashBackSlash ~ appendSB()
      | 'b' ~ appendSB('\b')
      | 'f' ~ appendSB('\f')
      | 'n' ~ appendSB('\n')
      | 'r' ~ appendSB('\r')
      | 't' ~ appendSB('\t')
      | Unicode ~> { code => sb.append(code.asInstanceOf[Char]); () }
  )

  def Unicode = rule { 'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16)) }

  // def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }
  // def ws(c: Char) = rule { c ~ WhiteSpace }
  // val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  val QuoteBackslash = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"
  //
  //----
}