import org.json4s.jackson.JsonMethods.parse
import scala.io.Source

object Stemmer {

  case object Rules {

    val plural = "plural"
    val feminine = "feminine"
    val adverb = "adverb"
    val augmentative= "augmentative"
    val noun = "noun"
    val verb = "verb"
    val vowelRemoval = "vowelRemoval"

  }

  def loadRules(): Array[Rule] = {

    val jsonSource = Source.fromInputStream(getClass.getResourceAsStream("/stemming_rules.json"))
      .getLines()
      .mkString("")

    val rules = parse(jsonSource) \ "rules"

    Array(new Rule(rules \ Rules.plural),
          new Rule(rules \ Rules.feminine),
          new Rule(rules \ Rules.adverb),
          new Rule(rules \ Rules.augmentative),
          new Rule(rules \ Rules.noun),
          new Rule(rules \ Rules.verb),
          new Rule(rules \ Rules.vowelRemoval))

  }

  def removeAccents(word: String): String = {

    word map {
      case 'á' => 'a'
      case 'ã' => 'a'
      case 'é' => 'e'
      case 'í' => 'i'
      case 'ó' => 'o'
      case 'õ' => 'o'
      case 'ú' => 'u'
      case  c: Char => c
    }

  }

  def applyRules(word: String, rules: Array[Rule], step: Int) : String = {

    if (step >= rules.length) return removeAccents(word)

    val rule = rules(step)
    val (ruleApplied, stem) = rule.evaluate(word)

    if (rule.name == Rules.noun && ruleApplied) {
      return removeAccents(stem)
    }

    if (rule.name == Rules.verb && ruleApplied) {
      return removeAccents(stem)
    }

    applyRules(stem, rules, step + 1)
  }

}
