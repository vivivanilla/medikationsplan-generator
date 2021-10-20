package endpoint

import sttp.tapir._
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe._
import io.circe.generic.auto._
import data.Medikationsplan
import cats.effect.IO
import sttp.tapir.server.http4s.Http4sServerInterpreter
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.blaze.server._
import cats.effect.IOApp
import cats.effect.ExitCode
import scala.concurrent.ExecutionContext.global

object Main extends IOApp {
  val baseEndpoint: Endpoint[Unit, Unit, Unit, Any] =
    endpoint.in("api" / "v1.0")

  val medikationsplanInput = jsonBody[Medikationsplan]

  def getXml(m: Medikationsplan): IO[Either[Unit, String]] =
    IO.pure(Right[Unit, String](m.asXml))
  val xmlEndpoint =
    baseEndpoint.in("xml").in(medikationsplanInput).out(stringBody)
  val xmlRoute: HttpRoutes[IO] =
    Http4sServerInterpreter[IO]().toRoutes(xmlEndpoint)(getXml _)

  val router = Router("/" -> xmlRoute).orNotFound

  def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](global)
      .bindHttp(8080, "localhost")
      .withHttpApp(router)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
