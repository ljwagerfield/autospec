package autospec

import autospec.ResponseValidatorSpecBase.TestInstruction
import autospec.ResponseValidatorSpecBase.TestInstruction.{RunAssertion, SimulateRequestFailure}
import cats.data.NonEmptyList
import org.scalactic.source
import autospec.RuntimeSymbolsIndexed._
import autospec.runtime._
import autospec.schema.{ApiDefinition, ApiId, ApplicationSchema, EndpointDefinition, HttpMethod}

abstract class ResponseValidatorSpecBase extends BaseSpec {
  val apiId: ApiId                 = ApiId("api")
  val apiDefinition: ApiDefinition = ApiDefinition(apiId, "foo", addRequestIdHeader = true)
  val path: String                 = "/"                                             // Path not important for ResponseValidator, so we can set all to the same.
  val method: HttpMethod           = HttpMethod.Get                                  // Method not important for ResponseValidator, so we can set all to the same.

  def checks(
    expected: Predicate*
  )(implicit pos: source.Position): TestInstruction = {
    def assertion(actual: (EndpointRequestIndex, EndpointRequestSymbolic, Set[Predicate])): Unit = {
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

    RunAssertion(assertion)
  }

  def testInlineSpec(endpoints: EndpointDefinition*)(requests: (EndpointRequestSymbolic, TestInstruction)*): Unit = {
    implicit val a: ApplicationSchema = ApplicationSchema(apiDefinition :: Nil, endpoints.toList)

    test(requests: _*)
  }

  def test(requests: (EndpointRequestSymbolic, TestInstruction)*)(implicit schema: ApplicationSchema): Unit = {
    val conditions = requests
      .zipWithIndex
      .map {
        case ((_, predicates), requestIndex) =>
          EndpointRequestIndex(requestIndex.toLong) -> predicates
      }
      .toMap

    val testPlan = ResponseValidatorDebugger.executePlanAndReturnCheckedConditions(schema, requests)

    testPlan.zipWithIndex.foreach {
      case (requestOpt, index) =>
        val requestIndex    = EndpointRequestIndex(index.toLong)
        val testInstruction = conditions(requestIndex)

        testInstruction match {
          case RunAssertion(assertion) =>
            val request = requestOpt.get // This can only be 'None' for 'SimulateRequestFailure'.
            val actual  = request.checks
            assertion((requestIndex, request.request, actual))

          case SimulateRequestFailure =>
          // NoOp
        }
    }
  }

}

object ResponseValidatorSpecBase {
  sealed trait TestInstruction

  object TestInstruction {

    case class RunAssertion(assertion: ((EndpointRequestIndex, EndpointRequestSymbolic, Set[Predicate])) => Unit)
      extends TestInstruction

    case object SimulateRequestFailure extends TestInstruction

  }

}
