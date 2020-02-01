package spike.schema

case class ApplicationSchema(apis: List[ApiDefinition], endpoints: List[EndpointDefinition]) {
  val apiMap: Map[ApiId, ApiDefinition] = apis.map(x => x.id -> x).toMap
  val endpointMap: Map[EndpointId, EndpointDefinition] = endpoints.map(x => x.id -> x).toMap

  def api(apiId: ApiId): ApiDefinition =
    apiMap.getOrElse(apiId, throw new Exception(s"Cannot find API '$apiId' in schema."))

  def endpoint(endpointId: EndpointId): EndpointDefinition =
    endpointMap.getOrElse(endpointId, throw new Exception(s"Cannot find endpoint '$endpointId' in schema."))
}