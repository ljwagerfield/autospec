package playground

import cats.effect.concurrent.Ref
import monix.eval.Task

class OpportunitiesRepository {
  private val items: Ref[Task, List[Opportunities]] = Ref.unsafe[Task, List[Opportunities]](Nil)

  def addOpportunities(opportunities: Opportunities): Task[Unit] =
    items.update(opportunities :: _) // Todo: optimisation -- use Queue and drop last item if over 'config.maxHistorySizeForRequestGenerator'.

  def getPreviousOpportunities(limit: Int): Task[List[Opportunities]] =
    items.get.map(_.take(limit))
}
