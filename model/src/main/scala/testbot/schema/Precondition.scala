package testbot.schema

import testbot.SchemaSymbols.Predicate

case class Precondition(predicate: Predicate, errorStatusCode: Int)
