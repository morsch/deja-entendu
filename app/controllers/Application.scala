package controllers


import models.{LastFm, Track, TrackWithSpotify}
import org.joda.time.DateTime
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._

import scala.concurrent.{Future, future}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}


object Application extends Controller {

  val taskForm = Form(
    "username" -> nonEmptyText
  )

  def index = Action { implicit request =>
    request.getQueryString("username") match {
      case Some(username) => Redirect(routes.Application.agoDefault(username))
      case None =>
        Ok(views.html.index(taskForm))
    }
  }

  def setUser = Action { implicit request =>
    taskForm.bindFromRequest.fold( // lol magic
      errors => BadRequest(views.html.index(taskForm)),
      name => Redirect(routes.Application.agoDefault(name))
    )
  }

  def at(user: String, date: String) = Action.async {
    val d: DateTime = DateTime.parse(date)
    Track.getTracks(user, d, d.plusDays(1)) map { t =>
      Ok(views.html.tracks(user, t, defaultTimespans.map(ts => (ts._1, ts._2, false))))
    }
  }

  def parseAgo(ago: String): Try[DateTime] = {
    val ex: Regex = "([0-9]*)([dmyw])-ago".r
    ex.findFirstMatchIn(ago) match {
      case None => Failure(new IllegalArgumentException)
      case Some(matches) =>
        if (matches.groupCount != 2) Failure(new IllegalArgumentException)
        else {
          try {
            val num: Int = Integer.parseInt(matches.group(1))

            matches.group(2) match {
              case "d" => Success(DateTime.now().minusDays(num))
              case "m" => Success(DateTime.now().minusMonths(num))
              case "y" => Success(DateTime.now().minusYears(num))
              case "w" => Success(DateTime.now().minusWeeks(num))
            }
          } catch {
            case e : Exception => Failure(e)
          }
        }
    }
  }

  val defaultTimespans: Seq[(String,String)] =
    List(("5y-ago", "Five years"), ("3y-ago", "Three years"), ("2y-ago", "Two years"), ("1y-ago", "One year"),
      ("9m-ago", "Nine months"), ("6m-ago", "Six months"), ("3m-ago", "Three months"), ("1m-ago", "One month"), ("10w-ago", "Ten weeks"),
      ("1000d-ago", "One <i>thousand</i> days"), ("7d-ago", "Seven days") )

  def agoDefault(user: String) =  ago(user, "1y-ago")

  def getTracksSpotify(user: String, d: DateTime): Future[Seq[TrackWithSpotify]] = {
    Track.getTracks(user, d.minusMinutes(20), d.plusHours(8))
  }

  def getTracksLastfm(user: String, d: DateTime): Future[Seq[Track]] = {
    LastFm.tracksFromTo(user, d.minusMinutes(10), d.plusHours(8))
  }

  def ago(user: String, ago: String) = Action.async {
    parseAgo(ago) match {
      case Success(d) =>
        val tracks: Future[Seq[TrackWithSpotify]] = getTracksSpotify(user, d)

        // any tracks available at the given time?
        def tracksAvailable: DateTime => Future[Boolean] = getTracksLastfm(user, _).map((tracks: Seq[Track]) =>
          tracks.size > 0
        )

        def addSelectionInfo = { x:(String,String) => (x._1, x._2, x._1.equals(ago)) }

        val futureSpans: Future[Seq[(String, String, Boolean)]] = Future.traverse(defaultTimespans) { span =>
          val dt = parseAgo(span._1).get
          tracksAvailable(dt).map(b => addSelectionInfo(span) -> (b || span._1.equals(ago)) )

        }.map(_.collect { case (sms, true) => sms})

        for {
          tracks <- tracks
          timespans <- futureSpans
        } yield Ok(views.html.tracks(user, tracks, timespans))

      case Failure(t) =>
        future { InternalServerError }
    }
  }

}