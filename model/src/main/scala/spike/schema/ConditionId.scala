package spike.schema

import spike.runtime.EndpointRequestId

case class ConditionId(endpointId: EndpointId, conditionType: ConditionType, conditionIndex: Int) {
  override def toString: String =
    s"${conditionType.toString.toLowerCase} #$conditionIndex on '${endpointId.value}' endpoint"

  def withProvenance(provenance: EndpointRequestId): ConditionIdWithProvenance =
    ConditionIdWithProvenance(
      this,
      provenance
    )
}
