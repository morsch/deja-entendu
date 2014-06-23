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

  val defaultTimespans: Seq[(String,String)] = List(("5y-ago", "Five years"), ("3y-ago", "Three years"), ("1y-ago", "One year"), ("6m-ago", "Six months"), ("1m-ago", "One month"), ("10w-ago", "Ten weeks"), ("7d-ago", "Seven days") )

  def agoDefault(user: String) =  ago(user, "1y-ago")

  def getTracksSpotify(user: String, d: DateTime): Future[Seq[TrackWithSpotify]] = {
    Track.getTracks(user, d.minusMinutes(10), d.plusHours(12))
  }

  def getTracksLastfm(user: String, d: DateTime): Future[Seq[Track]] = {
    LastFm.tracksFromTo(user, d.minusMinutes(10), d.plusHours(12))
  }

  def futureFilter[A](seq: Seq[A], fn: (A => Future[Boolean])): Seq[Future[A]] = {



    val futureFilter: (A) => Future[A] = {
      a =>
        val futureTuple: Future[(A, Boolean)] =
          fn(a)
            .map((b: Boolean) => (a, b))

        val ehh: Future[A] = futureTuple
          .filter((tuple: (A, Boolean)) => tuple._2)
          .flatMap((tuple: (A, Boolean)) => future { tuple._1 })

        ehh
    }

    val map: Seq[Future[A]] = seq.map(futureFilter)
    map
//    val sequence: Future[Seq[A]] = Future.sequence( map )
//    sequence
  }


  def ago(user: String, ago: String) = Action.async {
    parseAgo(ago) match {
      case Success(d) =>

        val tracks: Future[Seq[TrackWithSpotify]] = getTracksSpotify(user, d)

//        val timespansWithPredicate =
//          Future.sequence(
//            defaultTimespans.map(ts => (ts._1, ts._2, ts._1.equals(ago)))
//              .map({ tuple: (String, String, Boolean) =>
//              tuple ->
//                (parseAgo(tuple._1) match {
//                  case Success(d) => getTracksLastfm(user, d)
//                  case Failure(t) => future {
//                    Nil
//                  }
//                })
//                  .map(_.size > 0)
//                  .andThen({ case Success(x) => println(tuple._1 + ": " + x)})
//            }) map { twa => twa._2 map {
//              (twa._1, _)
//            }
//            }
//          )
//
//        val timespans: Future[Seq[(String, String, Boolean)]] = timespansWithPredicate.map(t => t.filter(_._2).map(_._1))


        // any tracks available at the given time?
        def tracksAvailable: DateTime => Future[Boolean] = getTracksLastfm(user, _).map((tracks: Seq[Track]) =>
          tracks.size > 0
        )

        def addSelectionInfo = { x:(String,String) => (x._1, x._2, x._1.equals(ago)) }


        val futureSpans: Future[Seq[(String, String, Boolean)]] = Future.traverse(defaultTimespans) { span =>
          val dt = parseAgo(span._1).get
          tracksAvailable(dt).map(b => addSelectionInfo(span) -> b)

        }.map(_.collect { case (sms, true) => sms})


        for {
          tracks <- tracks
          timespans <- futureSpans
        } yield Ok(views.html.tracks(user, tracks, timespans))


      case Failure(t) =>
        future { InternalServerError }
    }
  }

//  def ago(user: String, ago: String) = Action.async {
//    parseAgo(ago) match {
//      case Success(d) =>
//
//        //
//
//
//
//        //
//
//        // the currently selected tracks with spotify ids
//        val tracks: Future[Seq[TrackWithSpotify]] = getTracksSpotify(user, d)
//
//        // any tracks available at the given time?
//        def tracksAvailable: DateTime => Future[Boolean] = getTracksLastfm(user, _).map((tracks: Seq[Track]) =>
//          tracks.size > 0
//        )
//
//        // mark which of the given timespans is selected
//        def addSelectionInfo: Future[(String, String)] => Future[(String, String, Boolean)] =
//          _.map( tuple => (tuple._1, tuple._2, tuple._1.equals(ago)) )
//
//        // the timespans that are available for the user
//        val availableTimespans: Seq[Future[(String, String)]] =
//          futureFilter(defaultTimespans, (tuple: (String, String)) => tracksAvailable(parseAgo(tuple._1).get))
//
//        // available timespans for the user including selection info
//        val selectedTimespans: Seq[Future[(String, String, Boolean)]] = availableTimespans.map( addSelectionInfo )
//
//        val futureResult: Future[SimpleResult] = for {
//          tracks <- tracks
//          timespans <- selectedTimespans
//        } yield Ok(views.html.tracks(user, tracks, timespans))
//
//        futureResult andThen {
//          case Success(x) => println(x)
//          case Failure(t) => println(t)
//        } fallbackTo future { InternalServerError }
//      case Failure(t) =>
//        future { InternalServerError }
//    }
//  }

}