** Please note that this project is a work in progress. At the time I would but it's completeness at 50% as a rough estimate. However it can't do anything useful yet. **

This is a web-based service (can also be used as a library) to generate medication schedule printouts conforming to nationwide German standards (bundeseinheitlicher Medikationsplan). The specification for this kind of document can be found [here](https://update.kbv.de/ita-update/Verordnungen/Arzneimittel/BMP/EXT_ITA_VGEX_BMP_Anlage3.pdf). You may look at some examples for valid printouts [here](https://download.hl7.de/ukf/Testfaelle_ukf201.pdf).

Currently the incomplete output of this project looks like this:

![medication schedule example](example.svg "Medikationsplan")

Additional test cases can be found at [HL7](https://wiki.hl7.de/index.php?title=IG:Ultrakurzformat_Patientenbezogener_Medikationsplan): [XML-Data](http://download.hl7.de/ukf/Testfaelle_ukf201.zip), [Printouts](http://download.hl7.de/ukf/Testfaelle_ukf201.pdf)

# Credits

* [Tapir](https://github.com/softwaremill/tapir) REST endpoint abstraction
* [http4s](https://github.com/http4s/http4s) web framework
* [Scalatags](https://github.com/com-lihaoyi/scalatags) XML/SVG templating
* [Circe](https://github.com/circe/circe) JSON databinding library
* [Okapi Barcode](https://github.com/woo-j/OkapiBarcode) 2D barcode generation
* [Apache Batik](https://xmlgraphics.apache.org/batik/) SVG to PDF conversion
