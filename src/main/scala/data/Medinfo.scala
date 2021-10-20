package data

import cats.effect.IO
import org.http4s.client.Client
import org.http4s.blaze.client.BlazeClientBuilder
import scala.concurrent.ExecutionContext
import org.http4s.EntityDecoder
import org.http4s.circe._
import io.circe.Json

case class Medinfo(
    pzn: String,
    name: Option[String],
    substance: Option[String],
    unit: Option[String]
)

trait MedinfoResolver {
  def lookupPzn(pzn: String)(using ec: ExecutionContext): IO[Medinfo]
}

object ScrapeAuResolver extends data.MedinfoResolver {
  def lookupPzn(pzn: String)(using ec: ExecutionContext): IO[Medinfo] = {
    val call: IO[Json] = BlazeClientBuilder[IO](ec).resource.use { client => client.expect[Json](s"https://www.apotheken-umschau.de/ajax/search/drugs/auto/?query=$pzn") }
    call.map{ res =>
        val meds: Seq[Json] = res.hcursor.downField("results").focus.flatMap(_.asArray).toSeq.flatten
        val med: Option[Json] = meds.filter(_.hcursor.downField("pzn").as[String].map(_.equals(pzn)).getOrElse(false)).headOption
        val name: Option[String] = med.flatMap(_.hcursor.downField("name").as[String].toOption)
        val unit: Option[String] = med.flatMap(_.hcursor.downField("packaging").downField("quantityUnit").as[String].toOption)
        val ingredients: Seq[Json] = med.toSeq.flatMap(_.hcursor.downField("activeIngredients").focus.flatMap(_.asArray).toSeq.flatten)
        val ingredient: Option[Json] = ingredients.filter(_.hcursor.downField("correspondingId").as[Option[String]].map(_.isEmpty).getOrElse(false)).headOption
        val substance = ingredient.flatMap(_.hcursor.downField("name").as[String].toOption)
        Medinfo(pzn, name, substance, unit)
        }
  }
}
