package spike.runtime

import spike.schema.ApplicationSchema

case class TestPlanOld(schema: ApplicationSchema, paths: List[TestPathWithChecksOld])