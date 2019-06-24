package translators

trait Translator {
  def apply(prog: String): String
  def unapply(prog: String): String
}
