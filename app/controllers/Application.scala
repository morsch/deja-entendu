package controllers

import models.Track
import org.joda.time.DateTime
import play.api._
import play.api.libs.ws.WS
import play.api.mvc._

import scala.util.matching.Regex
import scala.util.matching.Regex.{Match, MatchIterator}
import scala.util.{Success, Failure, Try}

object Application extends Controller {

  def index = Action {
    Redirect(routes.Application.at("morsch", "1y-ago"))
  }

  def at(user: String, date: String) = Action {
    val d: DateTime = DateTime.parse(date)
    Ok(views.html.tracks(user, Track.getTracks(user, d, d.plusDays(1))))
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

  def ago(user: String, ago: String) = Action {
    val td: Try[DateTime] = parseAgo(ago)
    td match {
      case Success(d) =>
        Ok(views.html.tracks(user, Track.getTracks(user, d.minusMinutes(10), d.plusDays(1))))
      case Failure(t) =>
        InternalServerError
    }
  }
}