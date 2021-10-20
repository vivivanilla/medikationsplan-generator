package data

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;

import org.apache.batik.transcoder.Transcoder;
import org.apache.batik.transcoder.TranscoderException;
import org.apache.batik.transcoder.TranscoderInput;
import org.apache.batik.transcoder.TranscoderOutput;
import org.apache.fop.svg.PDFTranscoder;
import java.io.StringReader
import java.io.ByteArrayOutputStream

object PdfOut {
  def exportPdfPage(svg: String): Array[Byte] = {
    val reader = new StringReader(svg)
    val os = new ByteArrayOutputStream()
    val transcoder = new PDFTranscoder()
    val transcoderInput = new TranscoderInput(reader)
    val transcoderOutput = new TranscoderOutput(os)
    transcoder.transcode(transcoderInput, transcoderOutput)
    os.toByteArray
  }
}
