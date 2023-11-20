#' Create a KML export of circuits to import to other mapping solutions
#'
# FIXME: finish writing out the comments here
# FIXME: also create a test to make sure it's all correct.
export_kml <- function(map_data, event_longitude, event_latitude) {
  # Each KML file has to open with the following header.
  # Document tag is needed because there can only be one parent element.
  # Colours go #aabbggrr: where a is for alpha, b for blue, g for green, r for red.
  kml_header <- paste(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      "<kml xmlns=\"http://www.opengis.net/kml/2.2\">",
      "  <Document>",
      "    <Style id=\"eventLocation\">",
      "      <IconStyle>",
      "        <color>ff0000ff</color>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"noDisconnections\">",
      "      <IconStyle>",
      "        <color>ff00ff00</color>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"disconnections\">",
      "      <IconStyle>",
      "        <color>ff0000ff</color>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>"
    ),
    collapse = "\n"
  )

  # Create KML for event location.
  kml_event_location <- kml_snippet("Event location", event_longitude, event_latitude, "#eventLocation")
  # Create KML for circuits: apply 1 means apply by row.
  kml_circuits <- paste(apply(map_data, 1, kml_snippet_from_row), collapse = "\n")
  kml_body <- paste(kml_event_location, kml_circuits, sep = "\n")
  # FIXME: Add concentric circles.
  # FIXME: Add save button for the power lines?

  # Each KML has to end with the following footer.
  kml_footer <- "  </Document>\n</kml>"
  kml_output <- paste(c(kml_header, kml_body, kml_footer), collapse = "\n")
  return(kml_output)
}

#' From each row within the map_data row, create KML snippet
#' using its postcode, percentage of disconnections, longitude and latitude.
kml_snippet_from_row <- function(row) {
  # FIXME: Change the colour depending on percentage of disconnections.
  name <- paste0("Postcode ", row[["s_postcode"]])
  if (row[["num_disconnects"]] == 0) {
    styleUrl <-"#noDisconnections"
  } else {
    styleUrl <-"#disconnections"
  }
  kml <- kml_snippet("", row[["lon"]], row[["lat"]], styleUrl, paste("Postcode", row[["s_postcode"]]))
  return(kml)
}

#' Create KML snippet from name, longitude, latitude, styleUrl and description.
# FIXME: Fill out these descriptions.
kml_snippet <- function(name, lon, lat, styleUrl, description = "") {
  snippet <- paste(
    c(
      "    <Placemark>",
      paste0("      <name>", name, "</name>"),
      paste0("      <description>", description, "</description>"),
      paste0("      <styleUrl>", styleUrl, "</styleUrl>"),
      "      <Point>",
      paste0("        <coordinates>", lon, ",", lat, "</coordinates>"),
      "      </Point>",
      "    </Placemark>"
    ),
    collapse = "\n"
  )
  return(snippet)
}
