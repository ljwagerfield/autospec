package autospec.schema

import autospec.LocalSchemaSymbols.Predicate

case class Precondition(predicate: Predicate, errorStatusCode: Int)
