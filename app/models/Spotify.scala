package models

import java.util.Date

import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.ws.WS

import scala.concurrent._
import scala.util.Success

/**
 * Created by moritz on 20.06.14.
 */
object Spotify {
  final val spotifyWsUrl = "https://api.spotify.com/v1/search"

  var last:Long=0

  def addSpotifyid(track: Track): Future[TrackWithSpotify] = {

    val delay: Long = last + 30 - System.currentTimeMillis()
    println(s"delaying $delay, >>")
    if (delay > 0)
      blocking(Thread.sleep(delay))
    last=System.currentTimeMillis()

    WS.url(spotifyWsUrl)
      .withQueryString(("q", s"artist:${track.artist} track:${track.name}"), ("type", "track")).get()
      .andThen({ case _ => println("<<") })
      .map { resp => resp.json \ "tracks" \ "items" \\ "id"}
      .map { ids =>
      if (ids.size > 0) {
        TrackWithSpotify(track.artist, track.name, track.date, Some(ids(0).as[String]))
      } else {
        println(s"could not find spotify id for ${track.artist} - ${track.name} - " + new Date(track.date * 1000))
        TrackWithSpotify(track.artist, track.name, track.date, None)
      }
    }

  }
}
