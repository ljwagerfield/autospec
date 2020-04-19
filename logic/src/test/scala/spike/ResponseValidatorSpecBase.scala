package spike

import cats.data.NonEmptyList
import org.scalactic.source
import spike.RuntimeSymbols._
import spike.runtime._
import spike.schema.ApplicationSchema

abstract class ResponseValidatorSpecBase extends BaseSpec {
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

  def test(requests: (EndpointRequestSymbolic, ((TestPathRequestIndex, EndpointRequestSymbolic, Set[Predicate])) => Unit)*)(implicit schema: ApplicationSchema): Unit = {
    val testPathId = TestPathId("example-test")

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
