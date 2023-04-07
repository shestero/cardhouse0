package datamodel

case class RecValueTrivial(v: String) extends RecValue {
  override def default: String = v 
}
