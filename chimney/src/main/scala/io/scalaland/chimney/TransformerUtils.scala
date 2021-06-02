package io.scalaland.chimney

object TransformerUtils {

  /**
    * Used by macros. Calling Map.map(..) inside quasiquotes leads to cryptic compiler errors in case of
    * implicitly-converted transformer, this hack fixes it.
    * @see Testcase#001
    */
  def __addPrefix(m: Map[String, String], keyPrefix: String, valuePrefix: String): Map[String, String] =
    m.map { case (key, value) => (keyPrefix + "." + key, valuePrefix + "." + value) }
}
