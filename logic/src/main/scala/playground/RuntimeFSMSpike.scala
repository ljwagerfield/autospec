package playground

import cats.Endo
import com.github.ghik.silencer.silent

import scala.util.Random
import cats.implicits._

// Until it looks like a Dinosaur...
// [X] Make it run.
// [X] Make it return a complete layered graph.
// [X] Make it favour 'closer dependencies'.
// [ ] Make 'Request' not just 'EndpointId' but also take params.
@silent
object RuntimeFSMSpike extends App {
  type EndpointId = String
  type Request = EndpointId
  type State = String
  type EndpointWeight = Double
  type Predicate = State => Boolean
  case class Endpoint(name: EndpointId, preconditions: List[Predicate], action: Endo[State]) {
    def isCallable(state: State): Boolean =
      preconditions.forall(_(state))
  }

  /**
   * The maximum probability factor to apply to rarely seen endpoints compared to the most commonly seen endpoints.
   * For example, a factor of 10 says "if we've seen endpoint A once, and endpoint B every time, then give endpoint
   * A 10x the chance of being called than endpoint B".
   */
  val maxProbabilityFactor = 10
  val endpointWeightDecimalPlaces = 2 // How much precision to capture on the `1/(X/maxProbabilityFactor)` equation.
  val endpointWeightMultiplier = Math.pow(10, endpointWeightDecimalPlaces)
  val initialState: State = "<none>"
  val immutableStates = Set("<none>", "cancelled")
  val maxRequests = 100
  val endpoints = List(
    Endpoint(
      "create",
      List(

      ),
      _ => "draft"
    ),
    Endpoint(
      "cancel",
      List(
        x => !immutableStates.contains(x)
      ),
      _ => "cancelled"
    ),
    Endpoint(
      "editAnswers",
      List(
        x => Set("draft", "receiverDraft", "amending").contains(x)
      ),
      identity
    ),
    Endpoint(
      "setApprovals",
      List(
        x => !immutableStates.contains(x)
      ),
      identity
    ),
    Endpoint(
      "sendToCounterparty",
      List(
        x => !(immutableStates + "inviting" + "confirmed" + "agreed").contains(x)
      ),
      s => Map(
        "draft" -> "inviting",
        "receiverDraft" -> "amending",
        "amending" -> "amending",
      )(s)
    ),
    Endpoint(
      "accept",
      List(
        _ === "inviting"
      ),
      _ => "receiverDraft"
    ),
    Endpoint(
      "confirm",
      List(
        x => Set("amending", "confirmed").contains(x)
      ),
      s => Map(
        "amending" -> "confirmed",
        "confirmed" -> "agreed"
      )(s)
    )
  )

  def countRequestsAvg(): Int = {
    val counts =
    (0 until 1000).map { _ =>
      countRequests(initialState, Nil)
    }

    Math.round(counts.sum.toDouble / counts.size.toDouble).toInt
  }

  @scala.annotation.tailrec
  def countRequests(state: State, history: List[(State, Request)]): Int = {
    if (hasFinished(history)) {
      history.size
    }
    else {
      val request          = nextRequest(state, history)
      val invocation       = endpoints.find(_.name === request).get.action
      countRequests(
        invocation(state),
        history :+ (state, request)
      )
    }
  }

  def hasFinished(history: List[(State, Request)]): Boolean =
    history.map(_._2).toSet.size === endpoints.size

  println()
  println(s"Requests until completion: ${countRequestsAvg()}")
  println()
  println("(Lower is better)")

  def nextRequest(state: State, history: List[(State, Request)]): Request = {
    val callableEndpoints = endpoints.filter(_.isCallable(state))
    val weightedEndpoints = callableEndpoints.flatMap { endpoint =>
      val weight         = endpointWeight(state, history, endpoint)
      val weightDiscrete = Math.round(weight * endpointWeightMultiplier).toInt
      List.fill(weightDiscrete)(endpoint)
    }
    val chosenEndpoint = Random.nextInt(weightedEndpoints.length)
    weightedEndpoints(chosenEndpoint).name
  }

  // Todo: needs perfecting (so that we get a desirable probability for each edge, so it takes less time to explore all edges).
  def endpointWeight(state: State, history: List[(State, Request)], endpoint: Endpoint): EndpointWeight = {
    val count      = history.reverse.take(maxProbabilityFactor).count { case (state, _) => endpoint.isCallable(state) }
    val penalty    = count + 1
    val maxPenalty = maxProbabilityFactor + 1
    1D / (penalty.toDouble / maxPenalty.toDouble)
//    1D
  }
}
