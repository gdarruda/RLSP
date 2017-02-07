object App {

  def main(args: Array[String]): Unit = {

    val rules = Stemmer.loadRules()
    println(Stemmer.applyRules("menina".toLowerCase, rules, 0))

  }

}
