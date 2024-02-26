library(dplyr)

## To create a voltage label KML:
## 1. create a CSV file (for an example, see out_of_tool_processing/tests/voltage.csv),
## 2. uncomment out the code at the bottom of this file.

create_voltage_kml <- function(data) {
  kml_header <- paste(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      "<kml xmlns=\"http://www.opengis.net/kml/2.2\">",
      "  <Document>",
      "    <name>Manual pu voltages</name>",
      "    <Style id=\"110kV\">",
      "      <LabelStyle>",
      "        <color>ff0000ff</color>",
      "        <scale>1.7</scale>",
      "      </LabelStyle>",
      "      <IconStyle>",
      "        <Icon/>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"132kV\">",
      "      <LabelStyle>",
      "        <color>ff0000ff</color>",
      "        <scale>1.7</scale>",
      "      </LabelStyle>",
      "      <IconStyle>",
      "        <Icon/>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"275kV\">",
      "      <LabelStyle>",
      "        <color>ffff00aa</color>",
      "        <scale>1.7</scale>",
      "      </LabelStyle>",
      "      <IconStyle>",
      "        <Icon/>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"66kV\">",
      "      <LabelStyle>",
      "        <color>ff1872cc</color>",
      "        <scale>1.7</scale>",
      "      </LabelStyle>",
      "      <IconStyle>",
      "        <Icon/>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"33kV\">",
      "      <LabelStyle>",
      "        <color>ff00aa00</color>",
      "        <scale>1.7</scale>",
      "      </LabelStyle>",
      "      <IconStyle>",
      "        <Icon/>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"22kV\">",
      "      <LabelStyle>",
      "        <color>ffffaa55</color>",
      "        <scale>1.7</scale>",
      "      </LabelStyle>",
      "      <IconStyle>",
      "        <Icon/>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"11kV\">",
      "      <LabelStyle>",
      "        <color>ffffaa55</color>",
      "        <scale>1.7</scale>",
      "      </LabelStyle>",
      "      <IconStyle>",
      "        <Icon/>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"220kV\">",
      "      <LabelStyle>",
      "        <color>ffff0000</color>",
      "        <scale>1.7</scale>",
      "      </LabelStyle>",
      "      <IconStyle>",
      "        <Icon/>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"330kV\">",
      "      <LabelStyle>",
      "        <color>ff00aaff</color>",
      "        <scale>1.7</scale>",
      "      </LabelStyle>",
      "      <IconStyle>",
      "        <Icon/>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"400kV\">",
      "      <LabelStyle>",
      "        <color>ffffaa55</color>",
      "        <scale>1.7</scale>",
      "      </LabelStyle>",
      "      <IconStyle>",
      "        <Icon/>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"500kV\">",
      "      <LabelStyle>",
      "        <color>ff00ffff</color>",
      "        <scale>1.7</scale>",
      "      </LabelStyle>",
      "      <IconStyle>",
      "        <Icon/>",
      "      </IconStyle>",
      "    </Style>"
    ),
    collapse = "\n"
  )
  data <- data %>%
    rowwise %>%
    do({
        result <- as_tibble(.)
        result$kml_snippet <- create_placemark_kml(
          result$label,
          result$voltage,
          result$nominal_line_voltage,
          result$latitude,
          result$longitude
        )
        result
    }) %>%
    select(kml_snippet) %>%
    unlist()
  kml_body <- paste(data, collapse = "\n")
  kml_footer <- "  </Document>\n</kml>"
  kml_output <- paste(c(kml_header, kml_body, kml_footer), collapse = "\n")
  return(kml_output)
}

create_placemark_kml <- function(label, voltage, nominal_line_voltage, latitude, longitude) {
  placemark_kml <- paste(
    c(
     "    <Placemark>",
     paste0("      <name>", voltage, "</name>"),
     paste0("      <description>", label, "</description>"),
     paste0("      <styleUrl>#", nominal_line_voltage, "</styleUrl>"),
     "      <Point>",
     paste0("        <coordinates>", longitude, ",", latitude, "</coordinates>"),
     "      </Point>",
     "    </Placemark>"
    ),
    collapse = "\n"
  )
  return(placemark_kml)
}

## ## Code to uncomment
## data <- read.csv("voltage.csv", sep = ",")
## output <- create_voltage_kml(data)
## cat(output, file = "output.kml")
