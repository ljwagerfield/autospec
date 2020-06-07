package autospec.common.turing

case class TransitionTo[+S, +I, +O](write: Option[O], next: NextState[S, _ <: Either[Unit, Unit]])
