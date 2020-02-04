package spike.runtime

import spike.schema.ApplicationSchema

case class TestPlan(schema: ApplicationSchema, paths: List[TestPathWithChecks])

object TestPlan {
  def from(schema: ApplicationSchema, paths: List[TestPath]): TestPlan =
    TestPlan(
      schema,
      paths.map(TestPathWithChecks(schema, _))
    )

  def from(schema: ApplicationSchema, pathGenerator: TestPathGenerator): TestPlan =
    TestPlan.from(
      schema,
      pathGenerator(schema)
    )
}