package datamodel

import datamodel.Record.RecKey

object Record {
  type RecKey = String
}

case class Record(k: RecKey, v: RecValue) {
  
  def htmlVal(): String = if (v.default.contains("\n")) s"<pre>$v</pre>" else s"$v" 

  def html1(): String = s"<td class='ckey'>$k:</td><td class='cvalue'>${htmlVal()}</td>"
}
