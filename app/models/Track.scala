package models

import org.joda.time.DateTime
import play.api.libs.concurrent.Execution.Implicits._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

/**
 * Created by moritz on 19.06.14.
 */
case class Track(artist: String, name: String, date:Long)

case class TrackWithSpotify(artist: String, name: String, date: Long, spotifyId: String)

object Track {
  def getTracks(user: String, from: DateTime, to: DateTime): Seq[TrackWithSpotify] = {
    val fTracks: Future[Seq[Track]] = LastFm.tracksFromTo(user, from.getMillis / 1000, to.getMillis / 1000)

    fTracks andThen {
      case Success(tracks) =>  println(s"lastfm($user, $from, $to) returned ${tracks.size} tracks")
      case Failure(x) => println("fail")
    }

    val futureTracksWithSpotify: Future[Seq[TrackWithSpotify]] = fTracks.map { seq =>
      seq.map { track => Await.result(Spotify.addSpotifyid(track), Duration.Inf)}.flatten
    }

    Await.result(futureTracksWithSpotify, Duration.Inf)
  }
}
