package controllers

import models.Track
import org.joda.time.DateTime
import play.api._
import play.api.libs.ws.WS
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Redirect(routes.Application.tasks)

  }

  def tasks() = Action { request =>
    Ok(views.html.index(Track.all))
  }

  def at(user: String, date: String) = Action {
    val d: DateTime = DateTime.parse(date)
    Ok(views.html.index(Track.getTracks(user, d, d.plusDays(1))))
  }
}