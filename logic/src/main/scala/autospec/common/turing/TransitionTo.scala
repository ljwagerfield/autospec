package autospec.common.turing

case class TransitionTo[+S, +I, +O](write: Option[O], move: Option[Either[Unit, Unit]], next: Option[MachineState[S]])
