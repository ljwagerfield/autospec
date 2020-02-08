package spike.macros

import scala.reflect.macros.blackbox

object ClientMacros {
  // Todo: remove 'A' by putting all model types into 'model' project, and then having 'macros' reference 'model'.
  // This 'A' can only be 'EndpointRequest'
  def endpointRequest[A](): A = macro endpointRequestImpl[A]

  def endpointRequestImpl[A](c: blackbox.Context)(): c.Expr[A] = {
    import c.universe._
    val enclosingMethod = c.internal.enclosingOwner match {
      case m: MethodSymbol => m
      case _ =>
        c.abort(
          c.enclosingPosition,
          "Enclosing owner must be a method."
        )
    }

    val params = enclosingMethod.paramLists.headOption.getOrElse(Nil)

    val parameterMap =
      Apply(
        Select(
          reify(Map).tree,
          TermName("apply")
        ),
        params.map { param =>
          val toJson =
            if (typeOf[Int] =:= param.typeSignature)
              TermName("fromInt")
            else if (typeOf[Long] =:= param.typeSignature)
              TermName("fromLong")
            else if (typeOf[Double] =:= param.typeSignature)
              TermName("fromDouble")
            else if (typeOf[Float] =:= param.typeSignature)
              TermName("fromFloat")
            else if (typeOf[String] =:= param.typeSignature)
              TermName("fromString")
            else if (typeOf[Boolean] =:= param.typeSignature)
              TermName("fromBoolean")
            else
              c.abort(
                c.enclosingPosition,
                "Enclosing method contains a parameter whose type is not supported."
              )

          q"EndpointParameterName(${param.name.toString}) -> RuntimeSymbols.Literal(io.circe.Json.$toJson($param))"
        }
      )

    c.Expr(
      q"""
        EndpointRequest(
          EndpointId(${enclosingMethod.name.toString}),
          $parameterMap
        )
       """
    )
  }
}
