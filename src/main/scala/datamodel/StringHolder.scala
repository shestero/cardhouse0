package datamodel

trait StringHolder {
  def default: String

  override def toString: String = default
}
