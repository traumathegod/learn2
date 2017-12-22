object Esempio1 {



//    def max(interi: Array[Int]): Int = {
//      var massimo : Int = interi(0)
//      interi.foreach(intero => if (intero > massimo) massimo = intero)
//      massimo
//    }

    def max2(interi: Array[Int]): Int = {
      val massimo = interi.toList.reduceLeft(
        (a, b) => if (a > b) a else b)
      massimo
    }

    def main(args: Array[String]): Unit = {
      args.foreach(arg => println(arg))
      println("Max is " + max2(args.map(_.toInt)))
    }

}
