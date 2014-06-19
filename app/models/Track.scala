package models

import java.util.Date

import org.joda.time.{Instant, DateTime}
import play.Play
import play.api.Configuration
import play.api.libs.json._
import play.api.libs.ws.{Response, WS}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import play.api.libs.concurrent.Execution.Implicits._
import scala.util.{Failure, Success, Try}

/**
 * Created by moritz on 19.06.14.
 */
case class Track(artist: String, name: String, date:Long)

case class TrackWithSpotify(artist: String, name: String, date: Long, spotifyId: String)

object LastFm {
  final val lastFmWsUrl: String = "http://ws.audioscrobbler.com/2.0/"
  final val lastFmApiKey: String = Play.application().configuration().getString("lastFmApiKey") //"d21be117b1bc7bb44b2db2fc4c2e17e5"

  def tracksFromToResponse(user : String, from : Long, to : Long) : Future[Response] = {
    WS.url(lastFmWsUrl)
      .withQueryString(("method", "user.getrecenttracks"), ("user", user), ("api_key", lastFmApiKey), ("from", from.toString), ("to", to.toString), ("limit", "50"), ("format", "json")).get()
  }

  def responseToTracks(r : Response) : Seq[Track] = {
    Json.parse(r.body) \ "recenttracks" \ "track" match {
      case JsArray(elements) =>
        elements
          .filter(x => (x \ "@attr" \ "nowplaying").isInstanceOf[JsUndefined])
          .map(x =>
          Track((x \ "artist" \ "#text").as[String], (x \ "name").as[String], (x \ "date" \ "uts").as[String].toLong)
          )
      case _ =>
        Nil
    }
  }

  def tracksFromTo(user : String, from : Long, to : Long) : Future[Seq[Track]] = {
    tracksFromToResponse(user, from, to) map { resp => responseToTracks(resp) }
  }
}

object Spotify {
  final val spotifyWsUrl = "https://api.spotify.com/v1/search"

  def addSpotifyid(track: Track): Future[Option[TrackWithSpotify]] = {
    WS.url(spotifyWsUrl)
      .withQueryString(("q", s"artist:${track.artist} track:${track.name}"), ("type", "track")).get()
      .map { resp => (Json.parse(resp.body) \ "tracks" \ "items" \\ "id")}
      .map { ids =>
      if (ids.size > 0) {
        Some(TrackWithSpotify(track.artist, track.name, track.date, ids(0).as[String]))
      } else {
        println(s"could not find spotify id for ${track.artist} - ${track.name} - " + new Date(track.date * 1000))
        None
      }
    }
  }
}

object Track {

  def all(): Seq[TrackWithSpotify] = {
    val lastFmUsername: String = "morsch"
    val from: Long = DateTime.parse("2010-05-01").getMillis/1000
    val to: Long = DateTime.parse("2010-06-02").getMillis/1000

    val fTracks: Future[Seq[Track]] = LastFm.tracksFromTo(lastFmUsername, from, to)

    val futureTracksWithSpotify: Future[Seq[TrackWithSpotify]] = fTracks.map { seq =>
      seq.map { track => Await.result(Spotify.addSpotifyid(track), Duration.Inf)}.flatten
    }

    Await.result(futureTracksWithSpotify, Duration.Inf)
  }
}
