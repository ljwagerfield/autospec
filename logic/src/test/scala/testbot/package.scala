import autospec.schema.{ApiDefinition, ApiId, ApplicationSchema, EndpointDefinition, EndpointId}

package object autospec {
  val testApiId: ApiId = ApiId("foo")

  def currentMethodEndpointId: EndpointId =
    EndpointId(
      Thread.currentThread().getStackTrace()(2).getMethodName
    )

  def schemaFromObject(schemaObj: Any): ApplicationSchema = {
    val unboxedTypesWithValues = List(
      false.getClass      -> false,
      ' '.getClass        -> ' ',
      0.toByte.getClass   -> 0.toByte,
      0.toShort.getClass  -> 0.toShort,
      0.toInt.getClass    -> 0.toInt,
      0.toLong.getClass   -> 0.toLong,
      0.toFloat.getClass  -> 0.toFloat,
      0.toDouble.getClass -> 0.toDouble
    )
    val methods = schemaObj.getClass.getDeclaredMethods.distinctBy { method =>
      // De-duplicate methods (for some reason this method otherwise returns 2x of each method).
      s"${method.getName}(${method.getParameters.map(_.getType.getName).mkString(",")})"
    }

    ApplicationSchema(
      List(
        ApiDefinition(
          testApiId,
          "http://foo"
        )
      ),
      methods.toList.map { method =>
        val params =
          method.getParameters.toList.map { param =>
            val paramType = param.getType
            if (paramType.isPrimitive)
              unboxedTypesWithValues
                .collectFirst { case (clazz, value) if clazz.isAssignableFrom(paramType) => value}
                .getOrElse(throw new Exception(s"Primitives of type ${paramType.getName} are currently unsupported."))
            else
              null
          }

        method.invoke(schemaObj, params: _*) match {
          case x: EndpointDefinition => x
          case _                     => throw new Exception(s"All public methods on schema objects must contain ${classOf[EndpointDefinition].getSimpleName}.")
        }
      }
    )
  }
}
