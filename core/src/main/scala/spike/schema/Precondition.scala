package spike.schema

import spike.SchemaSymbols.Predicate

case class Precondition(predicate: Predicate, errorStatusCode: Int)
