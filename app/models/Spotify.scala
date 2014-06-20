package models

import java.util.Date

import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.ws.WS

import scala.concurrent.Future

/**
 * Created by moritz on 20.06.14.
 */
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
