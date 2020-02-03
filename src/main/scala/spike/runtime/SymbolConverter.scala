package spike.runtime

import spike.RuntimeSymbols.{Predicate => RPredicate}
import spike.SchemaSymbols.{Predicate => SPredicate}

object SymbolConverter {
  /**
   * Converts a schema predicate to a runtime predicate.
   *
   * @return None if the [[predicate]] references requests outside of the [[requestScope]].
   *         Some otherwise.
   */
  def convertToRuntimePredicate(requestScope: List[EndpointRequest], requestIndexOffset: Int, predicate: SPredicate): Option[RPredicate] = {

  }
}
