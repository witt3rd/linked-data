package linkedData

object Main {
  def main(args: Array[String]) : Unit = {
    val x = new NTriples("<foo> <bar> <baz> .").line.run()
    println(x)
  }  
}