package controllers

import models.{TrackWithSpotify, Track}
import org.joda.time.DateTime
import play.api.http.Writeable
import play.api.libs.json.{JsArray, JsObject, Json, Writes}
import play.api.mvc.{Action, Results, Controller}

/**
 * Created by moritz on 20.06.14.
 */
object Tracks extends Controller {

  def at(user:String, date:String) = Action {
    try {
      val d = DateTime.parse(date)

      val map = Track.getTracks(user, d, d.plusDays(1))
        .map { toJson }

      Ok(Json.obj(("result", JsArray.apply(map))))

    } catch {
      case ioe : Exception => InternalServerError
    }

  }

  def toJson(t: TrackWithSpotify) :  JsObject = {
    Json.obj(("artist", t.artist), ("title", t.name), ("date", t.date), ("spotifyId", t.spotifyId))
  }
}
