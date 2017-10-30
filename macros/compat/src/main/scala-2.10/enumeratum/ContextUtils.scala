package enumeratum

object ContextUtils {

  type Context = scala.reflect.macros.Context

  // In 2.10, the constants have Java boxed types at compile time for some reason
  type CTLong = java.lang.Long
  type CTInt  = java.lang.Integer
  type CTShort = java.lang.Integer
  type CTChar = java.lang.Character

  /**
    * Returns a TermName
    */
  def termName(c: Context)(name: String): c.universe.TermName = {
    c.universe.newTermName(name)
  }

  /**
    * Returns a companion symbol
    */
  def companion(c: Context)(sym: c.Symbol): c.universe.Symbol = sym.companionSymbol
}
