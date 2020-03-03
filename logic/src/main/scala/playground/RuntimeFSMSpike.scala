package playground

import cats.Endo
import com.github.ghik.silencer.silent

import scala.util.Random
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler

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
  val strandSize = 3
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
        (state, request) :: history
      )
    }
  }

  def hasFinished(history: List[(State, Request)]): Boolean =
    history.map(_._2).toSet.size === endpoints.size

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
  def findUniqueStrands(state: State, history: List[(State, Request)], uniqueStrands: Set[List[Request]], timeout: Int): (Int, Int) = {
    val timeoutLimit = 30000
    if (timeout === timeoutLimit) { // Give up after N iterations of not finding any new strands
      uniqueStrands.size -> (history.size - timeoutLimit) // Remove fruitless requests
    }
    else {
      val request           = nextRequest(state, history)
      val strand            = request :: history.take(strandSize - 1).map(_._2)
      val strandFixedLength = Some(strand).filter(_.length === strandSize)
      val invocation        = endpoints.find(_.name === request).get.action
      val newUniqueStrands  = strandFixedLength.fold(uniqueStrands)(uniqueStrands + _)
      findUniqueStrands(
        invocation(state),
        (state, request) :: history,
        newUniqueStrands,
        timeout = if (newUniqueStrands.size === uniqueStrands.size) timeout + 1 else 0
      )
    }
  }

//  println()
//  println(s"Requests until completion: ${countRequestsAvg()}")
//  println()
//  println("(Lower is better)")

  val start = System.currentTimeMillis()
  val (uniqueStrands, requestCount) = findUniqueStrandsAvg()
  val end = System.currentTimeMillis()
  println()
  println(s"Number of $strandSize-strands found: $uniqueStrands (in $requestCount requests)")
  println()
  println(s"Took ${(end - start) / 1000}s")

  def nextRequest(state: State, history: List[(State, Request)]): Request = {
    val callableEndpoints = endpoints.filter(_.isCallable(state))
    val weightedEndpoints = callableEndpoints.flatMap { endpoint =>
      List.fill(endpointWeight(state, history, endpoint))(endpoint)
    }
    val chosenEndpoint = Random.nextInt(weightedEndpoints.length)
    weightedEndpoints(chosenEndpoint).name
  }

  def endpointWeight(state: State, history: List[(State, Request)], endpoint: Endpoint): EndpointWeight = {
    val count      = history.take(endpointWeightMemory).count { case (state, _) => endpoint.isCallable(state) }
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
}
