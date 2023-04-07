import datamodel.*
import datamodel.Record.*
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir.server.ServerEndpoint

import java.io.File
import scala.collection.MapView
import scala.io.Source
import scala.util.Try
import cats.effect.*
import cats.syntax.all.*
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.http4s.blaze.server.BlazeServerBuilder
import sttp.client3.*
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.net.URI
import scala.concurrent.ExecutionContext

object Main extends IOApp {

  val dir = "data"

  def getListOfDirs(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isDirectory).filterNot(_.getName.startsWith("_")).toList
    } else {
      List.empty[File]
    }
  }

  def getListOfFiles(dir: String): List[File] = getListOfFiles(new File(dir))

  def getListOfFiles(d: File): List[File] = {
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).filterNot(_.getName.startsWith("_")).filter(_.getName.endsWith(".txt")).toList
    } else {
      List.empty[File]
    }
  }

  val cards: List[Card] = getListOfDirs(dir).flatMap { d =>
    getListOfFiles(d).map(file => datamodel.Card(file, d.getName))
  }

  println(s"${cards.size} card(s) loaded.")

  // cards is grouped by classes into decks
  val decks = cards.groupBy(_.cls).map { case (cls, cards) => cls -> Deck(cls, cards) }

  // to boost the card search speed by each parameter=value pair
  // (paraveter,value)=>(class,card_id)
  val index: Map[(String, String), Map[String, Set[String]]] = cards.flatMap {
    case Card(id, cls, content) => content.map { case Record(k, v) => (k.toString, v.toString) -> (cls, id) }
  }.groupMapReduce(_._1)(_._2.some.toSet)(_ ++ _).view.mapValues(_.groupMapReduce(_._1)(_._2.some.toSet)(_ ++ _)).toMap

  // ===

  val style =
    """
      |<head>
      |<style>
      |.card { border: 1px solid; border-color: #96D4D4; margin: 4px; }
      |.ckey { vertical-align: top; text-align: end; }
      |.cvalue { vertical-align: top; text-align: start; }
      |</style>
      |</head>
      |""".stripMargin + "\n"

  // show overview of classes
  val logic0: Unit => IO[Either[Unit, String]] = _ =>
    IO.pure(Right[Unit, String](style +
      "<h2>Content summary:</h2>\n" +
      decks.toSeq.sortBy(-_._2.size()).map { case (cls, deck) =>
        s"""\t<h3><a href="/$cls"><big>$cls</big></a> (${deck.size()})</h3>\n${deck.fhtml()}\n"""
      }.mkString("\n") + "\n"
    ))

  // return one card by global id
  def logicCard(id: String): IO[Either[Unit, String]] =
    IO.pure(Right[Unit, String](style + cards.filter(_.id == id).map(_.html1()).mkString("\n"))) // TODO

  // return class
  def logic1(cls: String): IO[Either[Unit, String]] =
    IO.pure(Right[Unit, String] {
      decks.get(cls) match {
        case Some(deck) =>
          style +
            s"<h3>$cls (${deck.size()})</h3>\n" + deck.fhtml() + "\n" +
            deck.html1() +
            s"""\n<hr/>\n<a href="/">Everything</a> &lt;&lt; $cls"""
        case _ =>
          s"No such class: <b>$cls</b>"
      }
    })

  // return:
  // 1) the card of the class
  // 2) the parameter's summary for the class (using factors)
  // 3) the parameter=value global search (using the index)
  def logic2(p1: String, p2: String): IO[Either[Unit, String]] =
    IO.pure(Right[Unit, String] {

      // if p1 is some class: (1) or (2)
      val op0: Option[String] = decks.get(p1).flatMap { deck =>
        val factors = deck.factors

        // class + card id
        val op1 = deck.getById(p2).map(_.html1(factors))

        // class + parameter
        val op2 = factors.get(p2).map { factor =>
          s"<h3>$p1: $p2</h3>\n" + deck.fhtml(p2, factor) +
            factor.collect { case (v, c) if c > 0 =>
              val filtered = deck.filter(p2)(_.default.compareToIgnoreCase(v) == 0)
              s"<h3>$p2=$v (${filtered.size()})</h3>\n<ol>\n" +
                filtered.cards.toSeq.sortBy(_.id).map("\t<li>" + _.brief() + "</li>").mkString("\n") +
                "</ol>\n"
            }.mkString("\n")
        }

        op1 |+| op2
      }

      // parameter + value
      val op3 = index.get(p1 -> p2).map { m =>
        m.flatMap {
          case (cls, ids) => s"<h3>$cls (${ids.size})</h3>\n" +: {
            val cf = for {
              deck <- decks.get(cls).toSeq
              id <- ids.toSeq.sorted
              card <- deck.getById(id)
            } yield card -> deck.factors // card.html1(deck.factors)

            val secondary: Iterable[String] = cf.map(_._1)
              .flatMap { case Card(id, cls, _) => index.get(cls -> id) }
              .reduceOption(_ |+| _)
              .getOrElse(Map.empty)
              .flatMap{ case (cls, ids) =>
                s"<h4>~~&gt; $cls ~~&gt;</h4>\n" +: {
                  for {
                    deck <- decks.get(cls).toSeq
                    id <- ids.toSeq.sorted
                    card <- deck.getById(id)
                  } yield card.html1(deck.factors)
                }
              }

            cf.map( _.html1(_) ) ++ secondary
          }
        }.mkString("\n")
      }

      (op0 |+| op3).map(style + _).getOrElse(s"Nothing found like $p1/$p2</b>") +
        s"""\n<hr/>\n<a href="/">Everything</a> &lt;&lt; <a href="/$p1">$p1</a> &lt;&lt; $p1/$p2"""
    })

  def logic3(cls: String, field: String, value: String): IO[Either[Unit, String]] =
    IO.pure(Right[Unit, String] {
      decks.get(cls) match {
        case Some(deck) =>
          val filtred = deck.filter(field)(_.default.equalsIgnoreCase(value))
          val ve = new URI(null, null, s"$value", null).toASCIIString()
          style +
            decks.get(field).flatMap(deck => deck.getById(value).map(_ -> deck.factors)).map(_.html1(_)).getOrElse("") +
            s"\n<h3>$cls general navigation:</h3>\n" +
            deck.fhtml() +
            s"""\n<p/>\n[<a href="/">reset</a>] Query: <a href="/$field/$ve">$field=$value</a> [<a href="">pin</a>].\n""" +
            filtred.html1(deck.factors) +
            s"""\n<hr/>\n<a href="/">Everything</a> &lt;&lt; <a href="/$cls">$cls</a> """ +
            s"""&lt;&lt; <a href="/$cls/$field">$cls/$field</a> &lt;&lt; $cls/$field/$ve"""
        case _ =>
          s"No such class: <b>$cls</b>"
      }
    })

  val rootEndpoint =
    endpoint.description("List all cards")
      .get.in("")
      .out(htmlBodyUtf8)
      .serverLogic(logic0)

  val classEndpoint =
    endpoint.description("List of specific class of objects")
      .get.in(path[String])
      .out(htmlBodyUtf8)
      .serverLogic(logic1 _)

  val inClassEndpoint =
    endpoint.description("List of objects in class")
      .get.in(path[String] / path[String])
      .out(htmlBodyUtf8)
      .serverLogic(logic2 _)

  val queryEndpoint =
    endpoint.description("List of cards that satisfied a simple query")
      .get.in(path[String] / path[String] / path[String])
      .out(htmlBodyUtf8)
      .serverLogic(logic3 _)

  val cardEndpoint =
    endpoint.description("Return the selected card")
      .get.in("card").in(query[String]("id"))
      .out(htmlBodyUtf8)
      .serverLogic(logicCard _)

  val endpoints = rootEndpoint :: classEndpoint :: inClassEndpoint :: queryEndpoint :: cardEndpoint :: Nil
  val routes: HttpRoutes[IO] = Http4sServerInterpreter[IO]().toRoutes(endpoints)

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  override def run(args: List[String]): IO[ExitCode] = {
    // starting the server
    BlazeServerBuilder[IO]
      .withExecutionContext(ec)
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(Router("/" -> routes).orNotFound)
      .resource
      .useForever
      .as(ExitCode.Success)
  }
}