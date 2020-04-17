package spike.schema

case class ConditionId(endpointId: EndpointId, conditionType: ConditionType, conditionIndex: Int) {
  override def toString: String =
    s"${conditionType.toString.toLowerCase} #$conditionIndex on '${endpointId.value}' endpoint"
}
