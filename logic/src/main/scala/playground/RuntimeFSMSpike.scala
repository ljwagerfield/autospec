package playground

import cats.Endo
import com.github.ghik.silencer.silent

import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import testbot.common.MathUtils.weightedRandom

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
  type EndpointWeight = Int
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
  val endpointWeightMemory = 10

  /**
   * Strand length = the length from 'where the first of the bug reproduction steps is made, excluding any earlier steps
   * that are implied from the first step' to 'where the bug is observed'. E.g. first step would be 'set FDA' not
   * 'create negotiation', as it's impossible to 'set FDA' without 'create negotiation' anyway.
   *
   * 3-stage bugs are supersets of 2-stage bugs, and 2-stage bugs are supersets of 1-stage bugs, and so on.
   *
   * Since most bugs are 3-stage and below, we report how many unique 3-stage paths we've tested.
   *
   * Of course, some bugs are more than 3-stage, and these are tested too: the 'stage-ness' is not a constraint in the
   * software, but merely a reporting method.
   */
  val strandSize = 4
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

  def findUniqueStrandsAvg(): (Int, Int) = {
    implicit val scheduler: Scheduler = Scheduler.traced

    val parallelism = 8
    val results =
      Task.gatherN(parallelism)(
        (0 until parallelism * 10).map { _ =>
          Task.evalAsync {
            findUniqueStrands(initialState, Nil, Set.empty, 0)
          }
        }
      ).runSyncUnsafe()

    val (uniqueStrands, requests) = results.unzip

    if (uniqueStrands.toSet.size =!= 1) {
      println(s"${Console.RED}ERROR: Different strands discovered, increase 'timeoutLimit'.")
      print(Console.RESET)
    }

    (uniqueStrands.head, Math.round(requests.sum.toDouble / requests.size.toDouble).toInt)
  }

  @scala.annotation.tailrec
  def findUniqueStrands(state: State, history: List[(Set[EndpointId], Request)], uniqueStrands: Set[List[Request]], timeout: Int): (Int, Int) = {
    val timeoutLimit = 100000
    if (timeout === timeoutLimit) { // Give up after N iterations of not finding any new strands
      uniqueStrands.size -> (history.size - timeoutLimit) // Remove fruitless requests
    }
    else {
      val (callableEndpoints, request) = nextRequest(state, history)
      val strand                       = request :: history.take(strandSize - 1).map(_._2)
      val strandFixedLength            = Some(strand).filter(_.length === strandSize)
      val invocation                   = endpoints.find(_.name === request).get.action
      val newUniqueStrands             = strandFixedLength.fold(uniqueStrands)(uniqueStrands + _)
      findUniqueStrands(
        invocation(state),
        (callableEndpoints, request) :: history,
        newUniqueStrands,
        timeout = if (newUniqueStrands.size === uniqueStrands.size) timeout + 1 else 0
      )
    }
  }

  def nextRequest(state: State, history: List[(Set[EndpointId], Request)]): (Set[EndpointId], Request) = {
    val callableEndpoints = endpoints.filter(_.isCallable(state))
    val chosenEndpoint    = weightedRandom(callableEndpoints)(endpointWeight(state, history, _)).get
    (callableEndpoints.map(_.name).toSet, chosenEndpoint.name)
  }

  def endpointWeight(state: State, history: List[(Set[EndpointId], Request)], endpoint: Endpoint): EndpointWeight = {
    val count      = history.take(endpointWeightMemory).count { case (callableEndpoints, _) => callableEndpoints.contains(endpoint.name)  }
    val penalty    = count + 1
    val maxPenalty = endpointWeightMemory + 1

    // Random (no biasing of less-frequently available endpoints)
    //         Empirical performance (lower is better): 20,000 requests to find all unique 3-strands.
    1

    // Relative (endpoint A that has appeared Nx less than another endpoint B will have Nx more chance
    //           of being called in this iteration than endpoint B).
    //           Empirical performance (lower is better): 10,000 requests to find all unique 3-strands.
    Math.round((1D / (penalty.toDouble / maxPenalty.toDouble)) * 100).toInt

    // Linear (least-frequent endpoint has Nx more chance of being called than most frequent endpoint,
    //         but only N/(N-1)x chance than second-least-frequent endpoint).
    //         Empirical performance (lower is better): 5,000 requests to find all unique 3-strands.
    maxPenalty - count
  }

  val start = System.currentTimeMillis()
  val (uniqueStrands, requestCount) = findUniqueStrandsAvg()
  val end = System.currentTimeMillis()
  println()
  println(s"Number of $strandSize-strands found: $uniqueStrands (in $requestCount requests)")
  println()
  println(s"Took ${(end - start) / 1000}s")
}
