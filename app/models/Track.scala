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

case class TrackWithSpotify(artist: String, name: String, date: Long, spotifyId: Option[String])

object Track {
  val limit: Int = 30

  def getTracks(user: String, from: DateTime, to: DateTime): Future[Seq[TrackWithSpotify]] = {
    val fTracks: Future[Seq[Track]] = LastFm.tracksFromTo(user, from, to)

    fTracks andThen {
      case Success(tracks) =>  println(s"lastfm($user, $from, $to) returned ${tracks.size} tracks")
      case Failure(x) => println("fail")
    }

    val futureTracksWithSpotify: Future[Seq[TrackWithSpotify]] = fTracks.map { seq =>
      val someTracks: Seq[Track] = seq.slice(0, limit)

      //someTracks.map { track => Await.result(Spotify.addSpotifyid(track), Duration.Inf)}
      someTracks.map { track => Spotify.addSpotifyid(track)}.map( f => Await.result(f, Duration.Inf))

    }

    futureTracksWithSpotify
  }
}
