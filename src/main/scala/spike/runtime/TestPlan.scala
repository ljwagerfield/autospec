package spike.runtime

import spike.schema.ApplicationSchema

case class TestPlan(schema: ApplicationSchema, paths: List[TestPathWithChecks])

object TestPlan {
  def apply(schema: ApplicationSchema, paths: List[TestPath]): TestPlan =
    TestPlan(
      schema,
      paths.map(TestPathWithChecks(schema, _))
    )

  def apply(schema: ApplicationSchema, pathGenerator: TestPathGenerator): TestPlan =
    TestPlan(
      schema,
      pathGenerator(schema)
    )
}