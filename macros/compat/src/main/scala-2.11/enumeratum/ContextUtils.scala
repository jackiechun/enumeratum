package enumeratum

object ContextUtils {

  type Context = scala.reflect.macros.blackbox.Context

  // Constant types
  type CTLong = Long
  type CTInt  = Int
  type CTShort = Int
  type CTChar = Char

  /**
    * Returns a TermName
    */
  def termName(c: Context)(name: String): c.universe.TermName = {
    c.universe.TermName(name)
  }

  /**
    * Returns a companion symbol
    */
  def companion(c: Context)(sym: c.Symbol): c.universe.Symbol = sym.companion
}
