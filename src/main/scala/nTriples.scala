package linkedData

import org.parboiled2._

class NTriples(val input: ParserInput) extends Parser {

  // Main parsed types
  trait Line       // A parsed line; can either be Blank, Comment, or Triple
  trait Subject    // A triple's subject; can either be a uriref or namednode
  trait Predicate  // A triple's predicate; can either be a uriref or namednode
  trait Object     // A triple's object; will always be a uriref

  case class UriRef(uri: String) extends Subject with Object with Predicate
  case class NamedNode(name: String) extends Subject with Predicate

  case class Blank() extends Line
  case class Comment(text: String) extends Line
  case class Triple(subj: Subject, pred: Predicate, obj: Object) extends Line

  // ntripleDoc ::= line*  

  // line ::= ws* (comment | triple) ? eoln  
  def line: Rule1[Line] = rule { blank | nonBlank }
  private def blank: Rule1[Line] = rule { zeroOrMore(ws) ~ EOI ~ push(Blank()) }
  private def nonBlank: Rule1[Line] = rule { (comment | triple) ~ EOI }

  // comment ::= '#' (character - ( cr | lf ) )*  
  private def comment: Rule1[Comment] = rule {
    '#' ~ zeroOrMore(ws) ~ capture(zeroOrMore(CharPredicate.Printable)) ~> (c => Comment(c))
  }

  // triple ::= subject ws+ predicate ws+ object ws* '.' ws*   
  // private def triple: Rule1[Triple] = rule {
  //   subject ~ oneOrMore(ws) ~ predicate ~ oneOrMore(ws) ~ obj ~ zeroOrMore(ws) ~ '.' ~ zeroOrMore(ws) ~>
  //     ((s,p,o) => Triple(s,p,o))
  // }
  private def triple: Rule1[Triple] = rule {
    zeroOrMore(ws) ~ subject ~ zeroOrMore(ANY) ~>
      ((s : Subject) => Triple(s, UriRef(""), UriRef("")))
  }

  // subject ::= uriref | namedNode   
  private def subject: Rule1[Subject] = rule {
    uriref | namedNode
  }

  // // predicate ::= uriref   
  // private def predicate: Rule1[Predicate] = rule {
  //   capture(zeroOrMore(CharPredicate.Printable)) ~ oneOrMore(ws) ~> (UriRef(_))
  // }

  // // object ::= uriref | namedNode | literal   
  // private def obj: Rule1[Object] = rule {
  //   capture(zeroOrMore(CharPredicate.Printable)) ~ oneOrMore(ws) ~> (UriRef(_))
  // }

  // uriref ::= '<' absoluteURI '>'  
  private def uriref: Rule1[UriRef] = rule {
    '<' ~ capture(zeroOrMore(!'>' ~ ANY)) ~> (UriRef(_))    
  }

  // namedNode ::= '_:' name  
  private def namedNode: Rule1[NamedNode] = rule {
    '_' ~ ':' ~ capture(name) ~> (NamedNode(_))    
  }

  // literal ::= '"' string '"'   
  private def literal = ???

  // absoluteURI ::= ( character - ( '<' | '>' | space ) )+   
  private def absoluteUri = ???

  // We don't need end-of-line stuff, since we are processing one line at a time
  // eoln ::= cr | lf | cr lf  
  // private def eoln = rule { cr | lf | cr ~ lf }  
  // cr ::= #xD /* US-ASCII carriage return - decimal 13 */  
  // private def cr = CharPredicate('\r')
  // lf ::= #xA /* US-ASCII linefeed - decimal 10 */   
  // private def lf = CharPredicate('\n')

  // ws ::= space | tab  
  private def ws = rule { space | tab }  

  // space ::= #x20 /* US-ASCII space - decimal 32 */   
  private def space = CharPredicate(' ')

  // tab ::= #x9 /* US-ASCII horizontal tab - decimal 9 */  
  private def tab = CharPredicate('\t')

  // string ::= character* with escapes. Defined in section Strings  
  private def string = ???

  // name ::= [A-Za-z][A-Za-z0-9]*   
  private def name = rule {
    CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)
  }

  // character ::= [#x20-#x7E] /* US-ASCII space to decimal 127 */ 
  private def character = { CharPredicate.Printable }
}