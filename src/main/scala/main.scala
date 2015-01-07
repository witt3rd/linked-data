package linkedData

import scala.collection.immutable
import scala.io.Source

object Main {
  def main(args: Array[String]) : Unit = {

    // unitTest()
    // return

    if (args.size < 1) {
      println("missing .nt file")
      return
    }

    val Array(ntFile) = args

    val lines = Source.fromFile(ntFile).getLines()
    parse(lines)
  }

  private def parse(lines: Iterator[String]) : Unit = {
    val triples = lines map {l => new NTriples(l).line.run()}    
    triples foreach println
  }

  private def unitTest() : Unit = {

    val ntDoc = 
    """

# This is a comment
<http://www.w3.org/2004/02/skos/core#Concept> <http://www.w3.org/2000/01/rdf-schema#label> "Concept"@en .
<http://www.w3.org/2004/02/skos/core#Concept> <http://www.w3.org/2000/01/rdf-schema#isDefinedBy> <http://www.w3.org/2004/02/skos/core> .
<http://www.w3.org/2004/02/skos/core#Concept> <http://www.w3.org/2004/02/skos/core#definition> "An idea or notion; a unit of thought."@en .
<http://www.w3.org/2004/02/skos/core#Concept> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Class> .
_:BX2Db3de8bfX3A149861d9206X3AX2D7ffe <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://www.w3.org/2004/02/skos/core#Concept> .
_:BX2Db3de8bfX3A149861d9206X3AX2D7ffe <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:BX2Db3de8bfX3A149861d9206X3AX2D7ffd .
    """

    parse(ntDoc.lines)

  }
}