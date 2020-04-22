package autospec.schema

import autospec.SchemaSymbols.Predicate

case class Precondition(predicate: Predicate, errorStatusCode: Int)
