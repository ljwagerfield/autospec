package spike.runtime

import spike.schema.ApplicationSchema

case class TestPlan(schema: ApplicationSchema, paths: List[TestPathWithChecks])