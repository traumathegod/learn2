import scala.io.Source

object fileParser {

  def main(args: Array[String]): Unit = {
    printer(args)
  }

  def printer(files: Array[String]): Unit = {

    if (files.length > 0) {

      val lines = Source.fromFile(files(0)).getLines().toList

      val longestLine = lines.reduceLeft(
        (a,b) => if (a>b) a else b)

      val maxWidth = lunghezzaLinee(longestLine)

      for (line <- lines) {
        val numSpaces = maxWidth - lunghezzaLinee(line)
        val padding = " " * numSpaces
        println(lunghezzaCounter(line) + " | " + padding + lines)
      }
//        println(line.length + " " + line)
    }

    else
      Console.err.println("Please enter filename")
  }

  def lunghezzaLinee(s:String): Int = s.length.toString.length

  def lunghezzaCounter(s:String): String = {
    if (s.length < 10 && s.length >= 0) {
      0 + s.length.toString
    }
    else s.length.toString
  }

}


