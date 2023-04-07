package datamodel

import datamodel.Record.RecKey

import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.MapView

import java.io.File
import scala.io.Source

case class Card(id: String, cls: String, content: Seq[Record]) {

  def select(key: RecKey): Seq[RecValue] = content.collect { case Record(k, v) if k == key => v }

  def webId: String =
    select("webid").headOption.map(_.default)
      .orElse(select("id").headOption.map(_.default))
      .getOrElse(id)

  val blacklist: Set[RecKey] = Set("id", "webid", "class") // ?

  def public: Seq[Record] = content.filterNot { case Record(k, _) => blacklist.contains(k) }

  def show() = {
    println(s"$id:")
    public.foreach { case Record(k, v) => println(s"\t$k\t: ${v.default}") }
    println
  }

  def stringOrEmpty(k: RecKey): String =
    content.collectFirst{ case Record(kk,v) if kk==k => v.toString }.getOrElse("")

  def brief(): String =
    s"""<div><a name="$id" href="/$cls/$id">$id ($cls)</a> """ +
       s"${stringOrEmpty("name")} ${stringOrEmpty("language")} ${stringOrEmpty("description")}</div>"

  // generate HTML for the card; may use deck factor
  def html1(factors: MapView[RecKey,Seq[(String,Int)]] = MapView.empty): String = (
    s"""<a name="$cls/$id"><div id="$cls">
       |<table class="card" id="$id">\n
       |\t<th colspan="2"><b>$cls</b> <a href="/$cls/$id">$webId</a></th>""".stripMargin
      +:
      public.groupBy(_.k).toSeq.map { case (k, rs0) =>
        val rs = rs0.filterNot(_.v.default.trim.isEmpty).distinct
        def a(v: String): String =
          if (v.startsWith("http://")||v.startsWith("https://"))
            s"""<a href="$v">$v</a>"""
          else {
            factors.get(k).flatMap(_.collectFirst{ case (w,c) if w==v => c }) match { // ? case ignore
              case Some(c) if /* c>1 && */ v.trim.nonEmpty =>
                //val ve = URLEncoder.encode(v, UTF_8.toString)
                val ve = v.replace("/","%2F") // %2f --> %252f
                val path = new URI(null, null,
                  s"/$cls/$k/$ve", null).toASCIIString()
                s"""<a href="$path">$v</a> <small>($c)</small>"""
              case _ => v
            }
          }

        val (ab, ae) = factors.get(k).map{ _ => s"""<a href="/$cls/$k">""" -> "</a>" }.getOrElse(""->"")
        s"""\t<tr><td class="ckey">$ab$k$ae:</td><td class="cvalue">""" + {
          if (rs.size == 1)
            a(rs.head.v.default)
          else
            "<ul>" + rs.map(r => "<li>" + a(r.htmlVal()) + "</li>").mkString + "</ul>"
        } +
          "</td></tr>"
      } :+
      "</table></div></a>\n"
    ).mkString("\n")

}

object Card {
  def apply(file: File, cls: String): Card = Card(
    file.getName.replace(".txt",""),
    cls,
    Source.fromFile(file).getLines.map(_.trim).foldLeft(Seq.empty[(String, String)] -> Option.empty[(String, List[String])]) {
      case ((m, Some((key, list))), ".") => (m :+ (key -> list.filter(_.nonEmpty).mkString("\n"))) -> None
      case ((m, Some((key, list))), line) => m -> Some(key -> (list :+ line.replaceAll("\\p{C}", " ").trim))
      case ((m, None), s"$key::") => m -> Some(key.trim -> List.empty)
      case ((m, None), s"$key:$value") => (m :+ (key.trim -> value.replaceAll("\\p{C}", "?").trim)) -> None
      case ((m, None), _) => m -> None
    }._1.map(_ -> RecValueTrivial(_)).map(Record.apply.tupled)
  )
}
