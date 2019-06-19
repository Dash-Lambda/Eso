package translators

object BrainPuff {
  def bpTobf(prog: String): String = prog
    .replaceAll("""\*gasp\*""", "[")
    .replaceAll("""\*pomf\*""", "]")
    .replaceAll("""pf""", "+")
    .replaceAll("""bl""", "-")
    .replaceAll("""b""", ">")
    .replaceAll("""t""", "<")
    .replaceAll("""!""", ".")
    .replaceAll("""\?""", ",")
  
  def bfTobp(prog: String): String = prog
    .replaceAll("[", "*gasp*")
    .replaceAll("]", "*pomf*")
    .replaceAll("+", "pf")
    .replaceAll("-", "bl")
    .replaceAll(">", "b")
    .replaceAll("<", "t")
    .replaceAll(".", "!")
    .replaceAll(",", "?")
}
