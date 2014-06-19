package controllers

import models.Track
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
}