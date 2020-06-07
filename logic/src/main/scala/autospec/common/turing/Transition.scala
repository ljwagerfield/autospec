package autospec.common.turing

import autospec.common.HasKey
import autospec.common.turing.TapeSymbol.{IOSymbol, LeftEndMarker, RightEndMarker}

sealed trait Transition[+S, +I, +O] {
  def from: TransitionFrom[S, I, O] = TransitionFrom(current, read)
  def to: TransitionTo[S, I, O]     = TransitionTo(write, next)
  def current: S
  def read: TapeSymbol[I, O]
  def write: Option[O]
  def next: NextState[S, _ <: Either[Unit, Unit]]
}

object Transition {

  implicit def hasKey[S, I, O]: HasKey[TransitionFrom[S, I, O], Transition[S, I, O]] =
    new HasKey[TransitionFrom[S, I, O], Transition[S, I, O]] {
      override def key: Transition[S, I, O] => TransitionFrom[S, I, O] = _.from
    }

  case class Normal[+S, I, O](
    current: S,
    read: IOSymbol[I, O],
    write: Option[O],
    next: NextState[S, Either[Unit, Unit]]
  ) extends Transition[S, I, O]

  case class FromLeftEnd[S, I, O](current: S, next: NextState[S, Right[Unit, Unit]]) extends Transition[S, I, O] {
    override def read: TapeSymbol[I, O] = LeftEndMarker
    override def write: Option[O]       = None
  }

  case class FromRightEnd[S, I, O](current: S, next: NextState[S, Left[Unit, Unit]]) extends Transition[S, I, O] {
    override def read: TapeSymbol[I, O] = RightEndMarker
    override def write: Option[O]       = None
  }

}
