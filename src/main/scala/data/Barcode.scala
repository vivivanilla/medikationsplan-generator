package data

import uk.org.okapibarcode.backend.DataMatrix
import uk.org.okapibarcode.output.SvgRenderer
import java.awt.Color
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

object Barcode {
    def encodeAsSvg(mp: Medikationsplan): String = {
        val barcode = new DataMatrix()
        val os = new ByteArrayOutputStream()
        val width = getWidth(mp)
        val renderer = new SvgRenderer(os, 100.0/width, Color.WHITE, Color.BLACK, false)
        barcode.setContent(mp.asXml)
        renderer.render(barcode)
        os.toString(StandardCharsets.UTF_8)
    }

    def getWidth(mp: Medikationsplan): Int = {
        val barcode = new DataMatrix()
        val os = new ByteArrayOutputStream()
        val renderer = new SvgRenderer(os, 1.0, Color.WHITE, Color.BLACK, false)
        barcode.setContent(mp.asXml)
        renderer.render(barcode)
        os.close()
        barcode.getWidth
    }
}
