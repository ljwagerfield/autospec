package autospec

import cats.data.NonEmptyList
import org.scalactic.source
import autospec.RuntimeSymbolsIndexed._
import autospec.runtime._
import autospec.schema.{ApiDefinition, ApiId, ApplicationSchema, EndpointDefinition, HttpMethod}

abstract class ResponseValidatorSpecBase extends BaseSpec {
  val apiId: ApiId                 = ApiId("api")
  val apiDefinition: ApiDefinition = ApiDefinition(apiId, "foo")
  val path: String                 = "/"            // Path not important for ResponseValidator, so we can set all to the same.
  val method: HttpMethod           = HttpMethod.Get // Method not important for ResponseValidator, so we can set all to the same.

  def checks(
    expected: Predicate*
  )(actual: (EndpointRequestIndex, EndpointRequestSymbolic, Set[Predicate]))(implicit pos: source.Position): Unit = {
    val (requestIndex, request, actualConditions) = actual
    val expectedConditions                        = expected.toSet
    val missing                                   = expectedConditions -- actualConditions
    val unexpected                                = actualConditions -- expectedConditions

    if (missing.nonEmpty || unexpected.nonEmpty) {
      val analysis = new StringBuilder()

      analysis.append(s"Request:       #${requestIndex.index}\n")
      analysis.append(s"Signature:     ${ScalaSymbolPrinter.print(request, requestIndex)}\n")

      NonEmptyList.fromList(unexpected.toList).foreach { unexpected =>
        analysis.append(s"Not in Spec:   ${unexpected.head}\n")
        unexpected.tail.foreach(item => analysis.append(s"               $item\n"))
      }

      NonEmptyList.fromList(missing.toList).foreach { missing =>
        analysis.append(s"Not in Result: ${missing.head}\n")
        missing.tail.foreach(item => analysis.append(s"               $item\n"))
      }

      analysis.append("Failed. \n")

      fail(analysis.toString())
    }
  }

  def testInlineSpec(endpoints: EndpointDefinition*)(
    requests: (EndpointRequestSymbolic, ((EndpointRequestIndex, EndpointRequestSymbolic, Set[Predicate])) => Unit)*
  ): Unit = {
    implicit val a: ApplicationSchema = ApplicationSchema(apiDefinition :: Nil, endpoints.toList)

    test(requests: _*)
  }

  def test(
    requests: (EndpointRequestSymbolic, ((EndpointRequestIndex, EndpointRequestSymbolic, Set[Predicate])) => Unit)*
  )(implicit schema: ApplicationSchema): Unit = {
    val conditions = requests
      .zipWithIndex
      .map {
        case ((_, predicates), requestIndex) =>
          EndpointRequestIndex(requestIndex.toLong) -> predicates
      }
      .toMap

    val testPath = requests.map(_._1).toList
    val testPlan = ResponseValidatorDebugger.generateTestPlan(schema, testPath)

    testPlan.zipWithIndex.foreach {
      case (request, index) =>
        val requestIndex = EndpointRequestIndex(index.toLong)
        val actual       = request.checks
        val expected     = conditions(requestIndex)

        expected((requestIndex, request.request, actual))
    }
  }

}
