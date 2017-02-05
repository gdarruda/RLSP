object App {

  def main(args: Array[String]): Unit = {

    val rules = Stemmer.loadRules()
    println(Stemmer.applyRules("menino", rules, 0))

  }

}
