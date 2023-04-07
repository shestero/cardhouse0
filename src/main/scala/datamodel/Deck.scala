package datamodel

import cats.syntax.semigroup.*
import cats.syntax.traverse.*

import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection
import scala.language.postfixOps
import datamodel.Card
import datamodel.Record.RecKey

import scala.collection.MapView
import scala.util.{Success, Try}

case class Deck[F[Card] <: Iterable[Card]](cls: String, cards: F[Card]) {

  val factorLevel = 128

  def size(): Int = cards.size

  val byId: Map[String, Option[Card]] = cards.groupBy(_.id).view.mapValues(_.headOption).toMap

  def getById(k: String): Option[Card] = byId.get(k).flatten

  def filter(key: RecKey)(f: RecValue => Boolean, default: Boolean = false): Deck[Iterable] =
    Deck(cls, cards.filter {
      _.select(key) match
        case Seq() => default
        case s => s.exists(f)
    })

  def foreach(f: Card => Unit): Unit = cards.foreach(f)

  lazy val factors: MapView[RecKey, Seq[(String, Int)]] =
    cards
      .flatMap(_.public)
      .groupMapReduce(_.k)(r => Map(r.v.toString.toLowerCase() -> 1))(_ |+| _)
      .filter { case (_, m) =>
        m.size <= factorLevel
      }
      .view
      .mapValues {
        vs =>
          val vst = vs.toSeq.traverse { case p@(v, _) => Try(v.toInt -> p) }
          vst match
            case Success(vsi) => vsi.toSeq.sortBy(_._1).map(_._2)
            case _ => vs.toSeq.sortBy(-_._2)
      }

  // generate HTML for 1 factor key
  def fhtml(k: RecKey, vs: Seq[(String, Int)]): String = {
    val total = vs.map(_._2).sum
    s"<div><b>$k:</b> " + vs.map { case (v, c) =>
      val path = new URI(null, null, s"/$cls/$k/$v", null).toASCIIString()
      val pc = if (total > 0) ", " + String.format("%.1f", 100.0 * c / total) + "%" else ""
      s"""<a href="$path">$v</a> <small>($c$pc)</small>"""
    }.mkString(" ") + "</div>"
  }

  // generate HTML for all factors
  def fhtml(): String =
    """<div id="factors">""" + "\n" +
      factors.map { case (k, vs) => fhtml(k, vs) }.mkString("\n") +
      "\n</div>"

  // generate HTML for all the deck
  def html1(factors: MapView[RecKey, Seq[(String, Int)]] = factors): String =
    "cards count: " + size() + "<br/>\n" + cards.toSeq.sortBy(_.id).map(_.html1(factors)).mkString("\n")
}
