import org.json4s._

case class SuffixRule(minSize: Int, replacement: String, suffix: String, exceptions: Set[String])

class Rule(json: JsonAST.JValue) {

  val baseSuffixes: Array[String] = (json \ "baseSuffixes")
    .children
    .map(suffix => suffix.values.toString)
    .toArray[String]

  val fullWordException: Boolean = (json \ "fullWordException")
    .asInstanceOf[JBool].value

  val minWordSize: Int = (json \ "minWordSize")
    .asInstanceOf[JInt].values.toInt

  val name: String = (json \ "name")
    .asInstanceOf[JString].values

  val suffixes: List[SuffixRule] = (json \ "suffixes")
    .children
    .map(suffixInfo => {
      SuffixRule((suffixInfo \ "minSize").asInstanceOf[JInt].values.toInt,
                 (suffixInfo \ "replacement").asInstanceOf[JString].values,
                 (suffixInfo \ "suffix").asInstanceOf[JString].values,
                 (suffixInfo \ "exceptions").children.map(_.asInstanceOf[JString].values).toSet[String])
    })


  def evaluateSuffixRule(word: String, suffixRule: SuffixRule): Boolean = {

    val validSuffix = word.endsWith(suffixRule.suffix)
    val stemLength = word.length - suffixRule.suffix.length + suffixRule.replacement.length > suffixRule.minSize

    if (validSuffix && stemLength){

      if (fullWordException) {
        ! suffixRule.exceptions.contains(word)
      } else {
        ! suffixRule.exceptions.exists(exception => word.endsWith(exception))
      }

    } else {
      false
    }

  }

  def evaluate(word: String) : (Boolean, String) = {

    if (word.length < minWordSize) return (false, word)

    if (baseSuffixes.nonEmpty) {
      val matchedSuffixCount = baseSuffixes
        .count(suffix => word.endsWith(suffix))

      if (matchedSuffixCount == 0) return (false, word)
    }

    val stem =  suffixes
      .filter(evaluateSuffixRule(word, _))
      .map(suffixRule => word.dropRight(suffixRule.suffix.length) + suffixRule.replacement)

    if (stem.nonEmpty) (true, stem.head)
    else (false, word)
  }

}