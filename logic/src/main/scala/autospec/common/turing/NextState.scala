package autospec.common.turing

sealed trait NextState[+S, +M]

object NextState {

  case class Terminate(nextState: TerminalState)                       extends NextState[Nothing, Nothing]
  case class Continue[S, M](nextState: Option[S], moveHead: Option[M]) extends NextState[S, M]

  object Continue {
    def right[S](nextState: S): Continue[S, Right[Unit, Unit]] = Continue(Some(nextState), Some(Right(())))
    def right[S]: Continue[S, Right[Unit, Unit]]               = Continue(None, Some(Right(())))
    def left[S](nextState: S): Continue[S, Left[Unit, Unit]]   = Continue(Some(nextState), Some(Left(())))
    def left[S]: Continue[S, Left[Unit, Unit]]                 = Continue(None, Some(Left(())))
    def here[S](nextState: S): Continue[S, Nothing]            = Continue(Some(nextState), None)
  }

}
