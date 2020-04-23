package autospec

import cats.data.NonEmptyList
import org.scalactic.source
import autospec.RuntimeSymbols._
import autospec.runtime._
import autospec.schema.{ApiDefinition, ApiId, ApplicationSchema, EndpointDefinition, HttpMethod}

abstract class ResponseValidatorSpecBase extends BaseSpec {
  val apiId: ApiId                 = ApiId("api")
  val apiDefinition: ApiDefinition = ApiDefinition(apiId, "foo")
  val path: String                 = "/" // Path not important for ResponseValidator, so we can set all to the same.
  val method: HttpMethod           = HttpMethod.Get // Method not important for ResponseValidator, so we can set all to the same.

  def checks(expected: Predicate*)(actual: (TestPathRequestIndex, EndpointRequestSymbolic, Set[Predicate]))(implicit pos: source.Position): Unit = {
    val (requestId, request, actualConditions) = actual
    val expectedConditions = expected.toSet
    val missing            = expectedConditions -- actualConditions
    val unexpected         = actualConditions -- expectedConditions

    if (missing.nonEmpty || unexpected.nonEmpty) {
      val analysis = new StringBuilder()

      analysis.append(s"Request:       #${requestId.requestIndex}\n")
      analysis.append(s"Signature:     ${ScalaSymbolPrinter.print(request, requestId.requestIndex)}\n")

      NonEmptyList.fromList(unexpected.toList).foreach { unexpected =>
        analysis.append(s"Not in Spec:   ${unexpected.head}\n")
        unexpected.tail.foreach { item =>
          analysis.append(s"               $item\n")
        }
      }

      NonEmptyList.fromList(missing.toList).foreach { missing =>
        analysis.append(s"Not in Result: ${missing.head}\n")
        missing.tail.foreach { item =>
          analysis.append(s"               $item\n")
        }
      }

      analysis.append("Failed. \n")

      fail(analysis.toString())
    }
  }

  def testInlineSpec(endpoints: EndpointDefinition*)(requests: (EndpointRequestSymbolic, ((TestPathRequestIndex, EndpointRequestSymbolic, Set[Predicate])) => Unit)*): Unit = {
    implicit val a: ApplicationSchema = ApplicationSchema(apiDefinition :: Nil, endpoints.toList)

    test(requests:_*)
  }

  def test(requests: (EndpointRequestSymbolic, ((TestPathRequestIndex, EndpointRequestSymbolic, Set[Predicate])) => Unit)*)(implicit schema: ApplicationSchema): Unit = {
    val testPathId = TestPlanId("example-test")

    val conditions = requests
      .zipWithIndex
      .map { case ((_, predicates), requestIndex) =>
        TestPathRequestIndex(testPathId, requestIndex) -> predicates
      }
      .toMap

    val testPath = requests.map(_._1).toList
    val testPlan = ResponseValidatorDebugger.generateTestPlan(schema, testPath)

    testPlan.zipWithIndex.foreach { case (request, requestIndex) =>
      val requestId = TestPathRequestIndex(testPathId, requestIndex)
      val actual    = request.checks
      val expected  = conditions(requestId)

      expected((requestId, request.request, actual))
    }
  }
}
