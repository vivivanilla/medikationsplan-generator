package data

import scalatags.Text.svgTags._
import scalatags.Text.svgAttrs._
import scalatags.Text.svgAttrs.attr
import scalatags.DataConverters._
import scalatags.Text.implicits._

object Ausdruck {
  val maxBlocksPerPage = 15

  def emptyPage(mp: Medikationsplan): Medikationsplan = Medikationsplan(
    mp.items.filter(_ match {
      case _: Medikationsplan.Block => false
      case _                        => true
    }),
    mp.version,
    mp.patchnum,
    mp.instanzId,
    None,
    None,
    mp.laenderkennzeichen
  )

  def splitPages(mp: Medikationsplan): Seq[Medikationsplan] = {
    val pages = mp.blocks
      .foldLeft(Seq(emptyPage(mp))) {
        case (split: Seq[Medikationsplan], elem: Medikationsplan.Block) => {
          val lastPage = split.head
          val spaceNeeded = elem.height
          val spaceLeft = maxBlocksPerPage - lastPage.blocks.map(_.height).sum
          val currentPage =
            if (spaceLeft >= spaceNeeded) lastPage else emptyPage(mp)
          Medikationsplan(
            currentPage.items ++ Seq(elem),
            currentPage.version,
            currentPage.patchnum,
            currentPage.instanzId,
            None,
            None,
            currentPage.laenderkennzeichen
          ) +: split.tail
        }
      }
      .reverse
    pages.zipWithIndex.map { case (p, i) =>
      Medikationsplan(
        p.items,
        p.version,
        p.patchnum,
        p.instanzId,
        Some(i + 1),
        Some(pages.size),
        p.laenderkennzeichen
      )
    }
  }

  def pageSvg(mp: Medikationsplan, barcodeSvg: String): String = {
    def rootElem = svg(
      attr("version") := "1.1",
      xmlns := "http://www.w3.org/2000/svg",
      width := 29.7.cm,
      height := 21.0.cm,
      fontFamily := "Arial, Helvetica, sans-serif"
    )(identBlock, adminBlock, carrierBlock, tableBlock, footerBlock)

    def identBlock =
      svg(width := 7.cm, height := 4.cm, x := 0.85.cm, y := 0.85.cm)(
        border,
        identName,
        pageNum
      )
    def adminBlock =
      svg(width := 16.7.cm, height := 4.cm, x := 7.85.cm, y := 0.85.cm)(border)
    def carrierBlock =
      svg(
        width := 4.cm,
        height := 4.cm,
        x := 24.85.cm,
        y := 0.85.cm,
        viewBox := "0 0 100 100"
      )(
        raw(barcodeSvg)
      )
    def tableBlock =
      svg(width := 28.cm, height := 14.cm, x := 0.85.cm, y := 5.15.cm)
    def footerBlock =
      svg(width := 28.cm, height := 1.cm, x := 0.85.cm, y := 19.15.cm)

    def border =
      rect(
        width := 100.pct,
        height := 100.pct,
        strokeWidth := 1.pt,
        stroke := "black",
        fillOpacity := 0
      )

    def identName =
      text(x := 0.125.cm, y := 1.cm, fontSize := 20.pt, fontWeight := "bold")(
        "Medikationsplan"
      )
    def pageNum = for {
      page <- mp.seite
      pageTotal <- mp.gesSeitenzahl
    } yield text(x := 0.125.cm, y := 2.cm, fontSize := 14.pt)(
      s"Seite $page von $pageTotal"
    )

    def patientIdentity = for (patient <- mp.patient) yield {
      val titelLen = patient.titel.map(_.length).map(_ + 1).getOrElse(0)
      val vornameLen = patient.vorname.length + 1
      val namenszusatzLen =
        patient.namenszusatz.map(_.length).map(_ + 1).getOrElse(0)
      val vorsatzwortLen =
        patient.vorsatzwort.map(_.length).map(_ + 1).getOrElse(0)
      val nachnameLen = patient.nachname.length

      val titel = patient.titel.map(_ + " ").getOrElse("")
      val vorname = patient.vorname + " "
      val namenszusatz = patient.namenszusatz.map(_ + " ").getOrElse("")
      val vorsatzwort = patient.vorsatzwort.map(_ + " ").getOrElse("")
      val nachname = patient.nachname

      val name = if (
        titelLen +vornameLen + vorsatzwortLen + namenszusatzLen + vorsatzwortLen + nachnameLen <= 74
      ) s"$titel$vorname$namenszusatz$vorsatzwort$nachname"
      else if (titelLen +vornameLen + vorsatzwortLen + vorsatzwortLen + nachnameLen <= 74)
        s"$titel$vorname$namenszusatz$vorsatzwort$nachname"
      else if (titelLen + vornameLen + vorsatzwortLen + nachnameLen <= 74)
          s"$titel$vorname$namenszusatz$nachname"
      else if (titelLen + vornameLen + nachnameLen <= 74)
          s"$titel$vorname$nachname"
      else if (vornameLen + nachnameLen <= 74)
          s"$vorname$nachname"
      else s"$vorname$nachname".substring(0,73) + "…"
    }

    val svgResult = rootElem.render
    s"<?xml version=\"1.0\" standalone=\"no\"?><!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n$svgResult"
  }
}
