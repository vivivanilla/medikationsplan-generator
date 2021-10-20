package data

import java.time.LocalDate
import java.time.LocalDateTime
import sttp.tapir.Schema
import sttp.tapir.Validator
import sttp.tapir.generic.auto._
import sttp.tapir.ValidationError
import scalatags.Text.all._
import java.time.format.DateTimeFormatter
import scalatags.Text.TypedTag
import io.circe.Decoder
import io.circe.HCursor
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto.{deriveEncoder, deriveDecoder}
import scala.util.Try
import scalatags.Escaping
import scalatags.generic.Namespace
import scalatags.generic.Attr

extension [T](validator: Validator[T])
  def liftOption: Validator[Option[T]] =
    Validator.custom((opt: Option[T]) =>
      opt.map(validator.apply).getOrElse(Nil)
    )

final case class Medikationsplan(
    items: Seq[Medikationsplan.Item],
    version: String = "026",
    patchnum: Option[Int],
    instanzId: String,
    seite: Option[Int],
    gesSeitenzahl: Option[Int],
    laenderkennzeichen: String = "de-DE"
) {
  def asXml: String = tag("MP")(
    attr("v") := version,
    attr("U") := instanzId,
    for (gs <- gesSeitenzahl) yield attr("a") := gs,
    for (s <- seite) yield attr("z") := s,
    attr("l") := laenderkennzeichen
  )(items.map(_.asXml)).render
  def blocks: Seq[Medikationsplan.Block] =
    items.flatMap[Medikationsplan.Block](_ match {
      case b: Medikationsplan.Block => Seq(b)
      case _                        => Seq.empty
    })
  def patient: Option[Medikationsplan.Patient] = items
    .flatMap[Medikationsplan.Patient](_ match {
      case p: Medikationsplan.Patient => Seq(p)
      case _                          => Seq.empty
    })
    .headOption
  def ausdruckender: Option[Medikationsplan.Ausdruckender] = items
    .flatMap[Medikationsplan.Ausdruckender](_ match {
      case p: Medikationsplan.Ausdruckender => Seq(p)
      case _                          => Seq.empty
    })
    .headOption
}

object Medikationsplan {
  sealed trait Item {
    def kind: String
    def asXml: TypedTag[String]
  }

  object Item {
    implicit val schema: Schema[Item] =
      Schema.oneOfUsingField[Item, String](_.kind, _.toString)(
        "Patient" -> Patient.schema,
        "Ausdruckender" -> Ausdruckender.schema,
        "Parameter" -> Parameter.schema,
        "Block" -> Block.schema
      )

    implicit val decoder: Decoder[Item] = Decoder.instance[Item](c =>
      c.downField("kind")
        .as[String]
        .map[Decoder[Item]] { kind =>
          kind match {
            case "Patient" => Decoder.derived[Patient].map(x => x: Item)
            case "Ausdruckender" =>
              Decoder.derived[Ausdruckender].map(x => x: Item)
            case "Parameter" => Decoder.derived[Parameter].map(x => x: Item)
            case "Block"     => Decoder.derived[Block].map(x => x: Item)
            case x =>
              Decoder.failed(DecodingFailure(s"Kind $x unknown", List.empty))
          }
        }
        .flatMap(_.apply(c))
    )
    implicit val encoder: Encoder[Item] = deriveEncoder[Item]

  }

  final case class Patient(
      vorname: String,
      nachname: String,
      versichertenId: Option[String],
      geburtsdatum: LocalDate,
      geschlecht: Option[Patient.Geschlecht],
      titel: Option[String],
      vorsatzwort: Option[String],
      namenszusatz: Option[String]
  ) extends Item {
    def kind = "Patient"
    def asXml = tag("P", true)(
      attr("g") := vorname,
      attr("f") := nachname,
      for (egk <- versichertenId) yield attr("egk") := egk,
      attr("b") := geburtsdatum.format(DateTimeFormatter.BASIC_ISO_DATE),
      for (s <- geschlecht) yield attr("s") := s.toString,
      for (t <- titel) yield attr("t") := t,
      for (v <- vorsatzwort) yield attr("v") := v,
      for (z <- namenszusatz) yield attr("z") := z
    )()
  }
  object Patient {
    enum Geschlecht {
      case M, W, X, D
    }

    implicit val schema: Schema[Patient] = Schema
      .derived[Patient]
      .validate(
        Validator.minLength(1).and(Validator.maxLength(45)).contramap(_.vorname)
      )
      .validate(
        Validator
          .minLength(1)
          .and(Validator.maxLength(45))
          .contramap(_.nachname)
      )
      .validate(
        Validator
          .pattern("""[A-Z]\d{9}""")
          .liftOption
          .contramap(_.versichertenId)
      )
      .validate(
        Validator
          .minLength(1)
          .and(Validator.maxLength(20))
          .liftOption
          .contramap(_.titel)
      )
      .validate(
        Validator
          .minLength(1)
          .and(Validator.maxLength(20))
          .liftOption
          .contramap(_.vorsatzwort)
      )
      .validate(
        Validator
          .minLength(1)
          .and(Validator.maxLength(20))
          .liftOption
          .contramap(_.namenszusatz)
      )

    implicit val encoderGeschlecht: Encoder[Geschlecht] =
      Encoder.instance(geschlecht => Json.fromString(geschlecht.toString))
    implicit val decoderGeschlecht: Decoder[Geschlecht] = Decoder.instance(c =>
      c.as[String]
        .flatMap(geschlecht =>
          Try(Geschlecht.valueOf(geschlecht)).toEither.left.map(error =>
            DecodingFailure.fromThrowable(error, List.empty)
          )
        )
    )
  }

  final case class Ausdruckender(
      lanr: Option[String],
      idf: Option[String],
      kik: Option[String],
      name: String,
      strasse: Option[String],
      postleitzahl: Option[String],
      ort: Option[String],
      telefon: Option[String],
      email: Option[String],
      ausdruckdatum: LocalDateTime
  ) extends Item {
    def kind = "Ausdruckender"
    def asXml = tag("A", true)(
      for (lanr <- lanr) yield attr("lanr") := lanr,
      for (idf <- idf) yield attr("idf") := idf,
      for (kik <- kik) yield attr("kik") := kik,
      attr("n") := name,
      for (s <- strasse) yield attr("s") := s,
      for (z <- postleitzahl) yield attr("z") := z,
      for (c <- ort) yield attr("c") := c,
      for (p <- telefon) attr("p") := p,
      for (e <- email) attr("e") := e,
      ausdruckdatum.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    )()
  }

  object Ausdruckender {
    implicit val schema: Schema[Ausdruckender] = Schema
      .derived[Ausdruckender]
      .validate(Validator.pattern("""\d{9}""").liftOption.contramap(_.lanr))
      .validate(Validator.pattern("""\d{7}""").liftOption.contramap(_.idf))
      .validate(Validator.pattern("""\d{9}""").liftOption.contramap(_.kik))
      .validate(
        Validator.minLength(1).and(Validator.maxLength(80)).contramap(_.name)
      )
      .validate(
        Validator
          .minLength(1)
          .and(Validator.maxLength(30))
          .liftOption
          .contramap(_.strasse)
      )
      .validate(
        Validator.pattern("""\d{5}""").liftOption.contramap(_.postleitzahl)
      )
      .validate(
        Validator
          .minLength(1)
          .and(Validator.maxLength(20))
          .liftOption
          .contramap(_.ort)
      )
      .validate(
        Validator
          .minLength(1)
          .and(Validator.maxLength(20))
          .liftOption
          .contramap(_.telefon)
      )
      .validate(
        Validator
          .minLength(1)
          .and(Validator.maxLength(80))
          .liftOption
          .contramap(_.email)
      )
  }

  final case class Parameter(
      allergien: Option[String],
      schwanger: Option[Boolean],
      stillend: Option[Boolean],
      gewicht: Option[Float],
      groesse: Option[Int],
      kreatinin: Option[Float],
      freitext: Option[String]
  ) extends Item {
    def kind = "Parameter"
    def asXml = tag("O", true)(
      for (ai <- allergien) yield attr("ai") := ai,
      for (p <- schwanger) yield attr("p") := p,
      for (b <- stillend) yield attr("b") := b,
      for (w <- gewicht) yield attr("w") := f"$w%.1f",
      for (h <- groesse) yield attr("h") := h.toString,
      for (c <- kreatinin) yield attr("c") := f"$c%.2f",
      for (x <- freitext) yield attr("x") := x
    )()
  }

  object Parameter {
    implicit val schema: Schema[Parameter] = Schema
      .derived[Parameter]
      .validate(
        Validator
          .minLength(1)
          .and(Validator.maxLength(50))
          .liftOption
          .contramap(_.allergien)
      )
      .validate(
        Validator.enumeration(List(true)).liftOption.contramap(_.schwanger)
      )
      .validate(
        Validator.enumeration(List(true)).liftOption.contramap(_.stillend)
      )
      .validate(
        Validator
          .min[Float](0.0, false)
          .and(Validator.max(999.0, false))
          .liftOption
          .contramap(_.gewicht)
      )
      .validate(
        Validator
          .min(10)
          .and(Validator.max(299))
          .liftOption
          .contramap(_.groesse)
      )
      .validate(
        Validator
          .min[Float](0.0, false)
          .and(Validator.max(10.0, false))
          .liftOption
          .contramap(_.kreatinin)
      )
      .validate(
        Validator
          .minLength(1)
          .and(Validator.maxLength(77))
          .liftOption
          .contramap(_.freitext)
      )
  }

  final case class Block(
      items: Seq[Block.Item],
      ueberschriftFreitext: Option[String],
      ueberschriftCodiert: Option[Int]
  ) extends Item {
    def kind = "Block"
    def asXml = tag("S")(
      for (t <- ueberschriftFreitext) yield attr("t") := t,
      for (c <- ueberschriftCodiert) yield attr("c") := c
    )(items.map(_.asXml))
    def height: Int = items
      .map(_.height)
      .sum + (if (ueberschriftFreitext.nonEmpty || ueberschriftCodiert.nonEmpty)
                1
              else 0)
  }

  object Block {
    val zwischenueberschriften: Map[String, String] = Map(
      "411" -> "Bedarfsmedikation",
      "412" -> "Dauermedikation",
      "413" -> "Intramuskuläre Anwendung",
      "414" -> "Besondere Anwendung",
      "415" -> "Intravenöse Anwendung",
      "416" -> "Anwendung under die Haut",
      "417" -> "Fertigspritze",
      "418" -> "Selbstmedikation",
      "419" -> "Allergiehinweise",
      "421" -> "Wichtige Hinweise",
      "422" -> "Wichtige Angaben",
      "423" -> "zu besonderen Zeiten anzuwendende Medikamente",
      "424" -> "zeitlich befristet anzuwendende Medikamente"
    )

    sealed trait Item extends Product with Serializable {
      def kind: String
      def asXml: TypedTag[String]
      def height: Int
    }

    implicit val itemSchema: Schema[Item] =
      Schema.oneOfUsingField[Item, String](_.kind, _.toString)(
        "Medikation" -> Medikation.schema,
        "Freitextzeile" -> Freitextzeile.schema,
        "Rezeptur" -> Rezeptur.schema
      )

    implicit val decoder: Decoder[Item] = Decoder.instance[Item](c =>
      c.downField("kind")
        .as[String]
        .map[Decoder[Item]] { kind =>
          kind match {
            case "Medikation" => Decoder.derived[Medikation].map(x => x: Item)
            case "Freitextzeile" =>
              Decoder.derived[Freitextzeile].map(x => x: Item)
            case "Rezeptur" => Decoder.derived[Rezeptur].map(x => x: Item)
            case x =>
              Decoder.failed(DecodingFailure(s"Kind $x unknown", List.empty))
          }
        }
        .flatMap(_.apply(c))
    )
    implicit val encoder: Encoder[Item] = deriveEncoder[Item]

    final case class Medikation(
        wirkstoff: Option[Seq[Medikation.Wirkstoff]],
        modPZN: Option[Int],
        arzneimittelname: Option[String],
        darreichungsformCode: Option[String],
        darreichungsformFreitext: Option[String],
        dosierungMorgens: Option[Medikation.Dosierung],
        dosierungMittags: Option[Medikation.Dosierung],
        dosierungAbends: Option[Medikation.Dosierung],
        dosierungNacht: Option[Medikation.Dosierung],
        dosierungFreitext: Option[String],
        einheitCodiert: Option[String],
        einheitFreitext: Option[String],
        hinweise: Option[String],
        behandlungsgrund: Option[String],
        zusatzzeile: Option[String]
    ) extends Item {
      def kind = "Medikation"
      def asXml = tag("M", wirkstoff.isEmpty)(
        for (p <- modPZN) yield attr("p") := p,
        for (a <- arzneimittelname) yield attr("a") := a,
        for (f <- darreichungsformCode) yield attr("f") := f,
        for (fd <- darreichungsformFreitext) yield attr("fd") := fd,
        for (m <- dosierungMorgens) yield attr("m") := m,
        for (d <- dosierungMittags) yield attr("d") := d,
        for (v <- dosierungAbends) yield attr("v") := v,
        for (h <- dosierungNacht) yield attr("h") := h,
        for (t <- dosierungFreitext) yield attr("t") := t,
        for (du <- einheitCodiert) yield attr("du") := du,
        for (dud <- einheitFreitext) yield attr("dud") := dud,
        for (i <- hinweise) yield attr("i") := i,
        for (r <- behandlungsgrund) yield attr("r") := r,
        for (x <- zusatzzeile) yield attr("x") := x
      )(wirkstoff.toSeq.flatten.map(_.asXml))
      def height = 1 + (if (zusatzzeile.nonEmpty) 1 else 0)
    }

    object Medikation {
      final case class Wirkstoff(
          wirkstoff: String,
          wirkstaerkeFreitext: Option[String]
      ) {
        def asXml = tag("W")(
          attr("w") := wirkstoff,
          for (s <- wirkstaerkeFreitext) yield attr("s") := s
        )()
      }

      object Wirkstoff {
        implicit val schema: Schema[Wirkstoff] = Schema
          .derived[Wirkstoff]
          .validate(
            Validator
              .minLength(1)
              .and(Validator.maxLength(80))
              .contramap(_.wirkstoff)
          )
          .validate(
            Validator
              .minLength(1)
              .and(Validator.maxLength(15))
              .liftOption
              .contramap(_.wirkstaerkeFreitext)
          )

        implicit val decoder: Decoder[Wirkstoff] = deriveDecoder[Wirkstoff]
        implicit val encoder: Encoder[Wirkstoff] = deriveEncoder[Wirkstoff]
      }

      type Dosierung = String
      val dosierungValidator = Validator
        .pattern(
          """([1-9]\d{0,3})|([1-9]\d,\d)|(\d,\d{1,2})|1/8|1/2|2/3|1/3|1/4|3/4"""
        )
        .liftOption

      implicit val schema: Schema[Medikation] = Schema
        .derived[Medikation]
        .validate(Validator.maxSize(3).contramap(_.wirkstoff))
        .validate(
          Validator
            .min(1)
            .and(Validator.max(99999999))
            .liftOption
            .contramap(_.modPZN)
        )
        .validate(
          Validator
            .minLength(1)
            .and(Validator.maxLength(50))
            .liftOption
            .contramap(_.arzneimittelname)
        )
        .validate(
          Validator
            .pattern("[A-Z]{3}")
            .and(Validator.maxLength(3))
            .liftOption
            .contramap(_.darreichungsformCode)
        )
        .validate(
          Validator
            .minLength(1)
            .and(Validator.maxLength(7))
            .liftOption
            .contramap(_.darreichungsformFreitext)
        )
        .validate(dosierungValidator.contramap(_.dosierungMorgens))
        .validate(dosierungValidator.contramap(_.dosierungMittags))
        .validate(dosierungValidator.contramap(_.dosierungAbends))
        .validate(dosierungValidator.contramap(_.dosierungNacht))
        .validate(
          Validator
            .minLength(1)
            .and(Validator.maxLength(20))
            .liftOption
            .contramap(_.darreichungsformFreitext)
        )
        .validate(
          Validator.pattern("[#0-9a-v]").liftOption.contramap(_.einheitCodiert)
        )
        .validate(
          Validator
            .minLength(2)
            .and(Validator.maxLength(20))
            .liftOption
            .contramap(_.einheitFreitext)
        )
        .validate(
          Validator
            .minLength(1)
            .and(Validator.maxLength(80))
            .liftOption
            .contramap(_.hinweise)
        )
        .validate(
          Validator
            .minLength(1)
            .and(Validator.maxLength(50))
            .liftOption
            .contramap(_.behandlungsgrund)
        )
        .validate(
          Validator
            .minLength(1)
            .and(Validator.maxLength(200))
            .liftOption
            .contramap(_.zusatzzeile)
        )

    }

    final case class Freitextzeile(freitext: String) extends Item {
      def kind = "Freitextzeile"
      def asXml = tag("X", true)(attr("t") := freitext)()
      def height = 1
    }
    object Freitextzeile {
      implicit val schema: Schema[Freitextzeile] = Schema
        .derived[Freitextzeile]
        .validate(
          Validator
            .minLength(1)
            .and(Validator.maxLength(200))
            .contramap(_.freitext)
        )
    }

    final case class Rezeptur(freitext: String, zusatzzeile: Option[String])
        extends Item {
      def kind = "Rezeptur"
      def asXml = tag("R", true)(
        attr("t") := freitext,
        for (x <- zusatzzeile) yield attr("x") := x
      )()
      def height = 1 + (if (zusatzzeile.nonEmpty) 1 else 0)
    }

    object Rezeptur {
      implicit val schema: Schema[Rezeptur] = Schema
        .derived[Rezeptur]
        .validate(
          Validator
            .minLength(1)
            .and(Validator.maxLength(200))
            .contramap(_.freitext)
        )
        .validate(
          Validator
            .minLength(1)
            .and(Validator.maxLength(200))
            .liftOption
            .contramap(_.zusatzzeile)
        )

    }

    implicit val schema: Schema[Block] = Schema
      .derived[Block]
      .validate(
        Validator
          .minLength(1)
          .and(Validator.maxLength(50))
          .liftOption
          .contramap(_.ueberschriftFreitext)
      )
      .validate(
        Validator
          .enumeration(zwischenueberschriften.keySet.toList)
          .liftOption
          .contramap(_.ueberschriftCodiert)
      )
  }

  val itemPattern = """^(patient)(ausdruckender)(parameter)?(block){0,23}$""".r
  implicit val schema: Schema[Medikationsplan] = Schema
    .derived[Medikationsplan]

  implicit val encoderLocalDate: Encoder[LocalDate] = Encoder.instance(date =>
    Json.fromString(date.format(DateTimeFormatter.BASIC_ISO_DATE))
  )
  implicit val decoderLocalDate: Decoder[LocalDate] = Decoder.instance(c =>
    c.as[String]
      .flatMap(date =>
        Try(
          LocalDate.parse(date, DateTimeFormatter.BASIC_ISO_DATE)
        ).toEither.left.map(DecodingFailure.fromThrowable(_, List.empty))
      )
  )

}

def tag(s: String, void: Boolean = false): ConcreteHtmlTag[String] =
  makeAbstractTypedTag[String](s, void, Namespace.htmlNamespaceConfig)
def attr(s: String, ns: Namespace = null, raw: Boolean = false) =
  Attr(s, Option(ns), raw)
