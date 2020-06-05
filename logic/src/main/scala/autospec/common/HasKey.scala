package autospec.common

trait HasKey[K, V] {
  def key: V => K
}
