#' Targetlist creates dataframes from xml files. Columns: Description, Std (Calibration level), conc (concentration), analyte, rdatum (collection date) and response (analyte area/IS area).
#'
#' @param x path to xml file(s)
#'
#' @return list
#' @export targetlist
#'
#' @examples
#' list1 <- list.files(pattern = "*.xml")
#' list1 <- lapply(list1, targetlist)
#' @import xml2 lubridate

targetlist <- function(x) {

# read_xml finns i xml2 och läser in xml-filers trädstruktur i R!
  xmlfile <- read_xml(x, options = NULL)

# Tar bort name space.
  xml_ns_strip(xmlfile)

# Skapar namn-vector.
  analyt <- xml_text(xml_find_all(xmlfile, "//SAMPLELISTDATA/SAMPLE/COMPOUND/@name"))

  # unique räknar upp alla unika element i vectorn name.
  # På så sätt får vi reda på hur många analyter som ingår i xml-filen.
  uanalyt <- unique(analyt)

  # Skapar en variabel som senare används för att replikera description nedan.
  l <- length(uanalyt)

  # Hittar alla analyskoncentrationer under PEAK/analconc. xml_double behövs för siffror.
  conc <- xml_double(xml_find_all(xmlfile, "//PEAK/@analconc"))
  cl <- length(conc)

  # Hittar alla stdconc.
  std <- xml_double(xml_find_all(xmlfile, "//COMPOUND/@stdconc"))

  # Hittar alla descriptions under SAMPLE/desc. xml_text behövs för text.
  desc <- xml_text(xml_find_all(xmlfile, "//SAMPLE/@desc"))

  # Hittar alla responsfaktorer
  respf <- xml_double(xml_find_all(xmlfile, "//SAMPLELISTDATA/SAMPLE/COMPOUND/PEAK/@response"))

  # Hittar analysdatum.
  datum <- xml_text(xml_find_all(xmlfile, "//DATASET/@creationdate"))

  # Parse date - talar om ordningen på grunddata. Gör om till POSIXct.
  rdatum <- parse_date_time(datum, "dmy")
  rdatum <- rep(rdatum, each = cl)


  # Replikerar description för att fylla hela df när den skapas.
  desc <- rep(desc, each = l)

  # Skapar df av de tre vectorerna vi skapat hittills.
  df <- data.frame(desc, std, conc, analyt, rdatum, respf)

  # Tar bort alla ISkoncentrationer ur df.
  df2 <- df[-grep("IS", df$analyt), ]

  # Med hjälp av tidyr:s spread går vi från data_long till data_wide.
  # Dvs name-kolumnen sprids ut på en kolumn för varje unikt element i ursprungskolumnen.
  # df_long <- spread(df2, analyt, conc)

  }
