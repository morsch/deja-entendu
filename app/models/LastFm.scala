package models

import org.joda.time.{Instant, DateTime}
import play.Play
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.ws.{Response, WS}

import scala.concurrent.Future
import scala.util.Failure

/**
 * Created by moritz on 20.06.14.
 */
object LastFm {
  final val lastFmWsUrl: String = "http://ws.audioscrobbler.com/2.0/"
  final val lastFmApiKey: String = Play.application().configuration().getString("lastFmApiKey")

  def tracksFromToCall(user : String, from : Long, to : Long) : Future[Response] = {
    WS.url(lastFmWsUrl)
      .withQueryString(("method", "user.getrecenttracks"), ("user", user), ("api_key", lastFmApiKey), ("from", from.toString), ("to", to.toString), ("limit", "100"), ("format", "json")).get()
  }

  def responseToTracks(r : Response) : Seq[Track] = {
    Json.parse(r.body) \ "recenttracks" \ "track" match {
      case JsArray(elements) =>
        elements
          .reverse
          .filter(x => (x \ "@attr" \ "nowplaying").isInstanceOf[JsUndefined])
          .map(x =>
          Track((x \ "artist" \ "#text").as[String], (x \ "name").as[String], (x \ "date" \ "uts").as[String].toLong)
          )
      case _ =>
        Nil
    }
  }


  def userRegisteredCall(user: String) : Future[Response] = {
    WS.url(lastFmWsUrl)
      .withQueryString(("method", "user.getinfo"), ("user", user), ("api_key", lastFmApiKey), ("format", "json")).get()
  }

  def userRegisteredParse(r: Response) : DateTime = {
    new DateTime((Json.parse(r.body) \ "user" \ "registered" \ "unixtime").as[String].toLong)
  }

  def userRegistered(user: String) : Future[DateTime] = {
    userRegisteredCall(user).map(userRegisteredParse)
  }

  def tracksFromTo(user : String, from : Long, to : Long) : Future[Seq[Track]] = {
    tracksFromToCall(user, from, to)
    .andThen {
      // case Success(x) => println(x.body)
      case Failure(x) => println(x)
    }
    .map { resp => responseToTracks(resp) }
  }

}
