package spike.schema

case class ConditionId(id: EndpointId, conditionType: ConditionType, conditionIndex: Int) {
  override def toString: String =
    s"${conditionType.toString.toLowerCase} #$conditionIndex on '${id.value}' endpoint"
}
