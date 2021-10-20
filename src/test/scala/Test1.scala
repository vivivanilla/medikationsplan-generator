import concurrent.ExecutionContext.Implicits.global
import cats.effect.unsafe.implicits.global
import org.scalatest._
import flatspec._
import matchers._
import matchers.should._
import org.scalatest.funsuite.AnyFunSuite
import data.Medikationsplan
import data.Barcode
import data.Medikationsplan.Block
import data.Medikationsplan.Block.Medikation
import data.ScrapeAuResolver
import data.Medinfo
import data.Ausdruck
import java.io.BufferedWriter
import java.io.FileWriter
import data.PdfOut
import java.io.FileOutputStream
import java.io.BufferedOutputStream

class Test extends AnyFunSuite with Matchers {

  test("JSON document decodes with Circe") {
    getMedikationsplan()
  }

  test("Medikationsplan is valid") {
    val errors = Medikationsplan.schema.validator(getMedikationsplan())
    errors shouldBe empty
  }

  test("Medikationsplan exports XML") {
    getMedikationsplan().asXml
  }

  test("Medikationsplan renders SVG barcode") {
    Barcode.encodeAsSvg(getMedikationsplan())
  }

  test("PZNs can be resolved") {
    getMedikationsplan().items.foreach {
      _ match {
        case block: Block =>
          block.items.foreach {
            _ match {
              case med: Medikation =>
                med.modPZN match {
                  case Some(_) => meds.contains(med)
                  case None    => ()
                }
              case _ => ()
            }
          }
        case _ => ()
      }
    }
  }

  test("Medikationsplan can be split into pages") {
    val pages = Ausdruck.splitPages(getMedikationsplan())
    pages should have length 1
    pages.head.seite should contain(1)
    all(pages.map(_.gesSeitenzahl)) should contain(1)
  }

  test("Medikationsplan can be rendered as SVG") {
    val mp = Ausdruck.splitPages(getMedikationsplan()).head
    val barcode = Barcode.encodeAsSvg(getMedikationsplan())
    val svg = Ausdruck.pageSvg(mp, barcode)

    val writer = new BufferedWriter(new FileWriter("example.svg"));
    writer.write(svg)
    writer.close()
  }

  test("Single page Medikationsplan can be converted to PDF") {
    val mp = Ausdruck.splitPages(getMedikationsplan()).head
    val barcode = Barcode.encodeAsSvg(getMedikationsplan())
    val svg = Ausdruck.pageSvg(mp, barcode)
    val pdf = PdfOut.exportPdfPage(svg)

    val writer = new BufferedOutputStream(new FileOutputStream("example.pdf"));
    writer.write(pdf)
    writer.close()
  }

  def getMedikationsplan(): Medikationsplan = {
    import io.circe.parser.decode
    import io.circe.generic.auto._
    import java.nio.file.{Files, Paths}
    import java.nio.charset.StandardCharsets

    val jsonText = Files.readString(
      Paths.get(this.getClass.getResource("/test1.json").getPath),
      StandardCharsets.UTF_8
    )

    val parsed = decode[Medikationsplan](jsonText)
    parsed should be a Symbol("right")
    parsed.toOption.get
  }

  val meds = List(
    Medinfo(
      "4213974",
      Some("RAMIPRIL STADA 5 mg Tabletten"),
      Some("Ramipril"),
      Some("Stück")
    ),
    Medinfo(
      "6453174",
      Some("HCT-1A Pharma 25 mg Tabletten"),
      Some("Hydrochlorothiazid"),
      Some("Stück")
    ),
    Medinfo(
      "4129423",
      Some("PLAVIX 75 mg Filmtabletten"),
      Some("Clopidogrel sulfat"),
      Some("Stück")
    ),
    Medinfo("1048888", None, None, None),
    Medinfo(
      "544757",
      Some("PROTAPHANE Penfill 100 I.E./ml Inj.-Susp. Patrone"),
      Some("Insulin-Isophan (human)"),
      Some("Milliliter")
    ),
    Medinfo("4877970", Some("NITRANGIN Pumpspray"), None, Some("Gramm")),
    Medinfo(
      "2083906",
      Some("VIVINOX Sleep Schlaftabletten stark"),
      Some("Diphenhydramin hydrochlorid"),
      Some("Stück")
    )
  )
}
