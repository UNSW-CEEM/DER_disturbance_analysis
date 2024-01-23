#' Create a Keyhole Markup Language (KML) export of circuits to import to other mapping solutions
#'
#' Points and shapes in KMLs are rendered from top to bottom in Google Earth. So features which should be rendered last
#' (i.e. on top of other elements) should be at the bottom of the file.
#'
#' Other resources
#' ===============
#' For more documentation on KML, see Google's documentation: https://developers.google.com/kml/documentation/kml_tut
#' For other possible icons to use, see http://kml4earth.appspot.com/icons.html
export_kml <- function(map_data,
                       event_longitude,
                       event_latitude,
                       zone_one_radius,
                       zone_two_radius,
                       zone_three_radius,
                       scaling = TRUE) {
  # TODO: Add in UFLS detection?

  # Each KML file has to open with the following header.
  # Document tag is needed because there can only be one parent element.
  # The styles are currently hardcoded in. Colours are defined by aabbggrr where a: alpha, b: blue, g: green, r: red.
  kml_header <- paste(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      "<kml xmlns=\"http://www.opengis.net/kml/2.2\">",
      "  <Document>",
      "    <name>Postcode disconnections from DERDAT</name>",
      "    <Style id=\"eventLocation\">",
      "      <IconStyle>",
      "        <color>ffffffff</color>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/paddle/ylw-stars-lv.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"noDisconnectionsTiny\">",
      "      <IconStyle>",
      "        <color>ff00ff00</color>",
      "        <scale>0.6</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"10disconnectionsTiny\">",
      "      <IconStyle>",
      "        <color>ff00ff33</color>",
      "        <scale>0.6</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"20disconnectionsTiny\">",
      "      <IconStyle>",
      "        <color>ff00ff66</color>",
      "        <scale>0.6</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"30disconnectionsTiny\">",
      "      <IconStyle>",
      "        <color>ff00ff99</color>",
      "        <scale>0.6</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"40disconnectionsTiny\">",
      "      <IconStyle>",
      "        <color>ff00ffcc</color>",
      "        <scale>0.6</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"50disconnectionsTiny\">",
      "      <IconStyle>",
      "        <color>ff00ffff</color>",
      "        <scale>0.6</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"60disconnectionsTiny\">",
      "      <IconStyle>",
      "        <color>ff00ccff</color>",
      "        <scale>0.6</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"70disconnectionsTiny\">",
      "      <IconStyle>",
      "        <color>ff0099ff</color>",
      "        <scale>0.6</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"80disconnectionsTiny\">",
      "      <IconStyle>",
      "        <color>ff0066ff</color>",
      "        <scale>0.6</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"90disconnectionsTiny\">",
      "      <IconStyle>",
      "        <color>ff0033ff</color>",
      "        <scale>0.6</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"100disconnectionsTiny\">",
      "      <IconStyle>",
      "        <color>ff0000ff</color>",
      "        <scale>0.6</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"noDisconnectionsSmall\">",
      "      <IconStyle>",
      "        <color>ff00ff00</color>",
      "        <scale>0.8</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"10disconnectionsSmall\">",
      "      <IconStyle>",
      "        <color>ff00ff33</color>",
      "        <scale>0.8</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"20disconnectionsSmall\">",
      "      <IconStyle>",
      "        <color>ff00ff66</color>",
      "        <scale>0.8</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"30disconnectionsSmall\">",
      "      <IconStyle>",
      "        <color>ff00ff99</color>",
      "        <scale>0.8</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"40disconnectionsSmall\">",
      "      <IconStyle>",
      "        <color>ff00ffcc</color>",
      "        <scale>0.8</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"50disconnectionsSmall\">",
      "      <IconStyle>",
      "        <color>ff00ffff</color>",
      "        <scale>0.8</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"60disconnectionsSmall\">",
      "      <IconStyle>",
      "        <color>ff00ccff</color>",
      "        <scale>0.8</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"70disconnectionsSmall\">",
      "      <IconStyle>",
      "        <color>ff0099ff</color>",
      "        <scale>0.8</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"80disconnectionsSmall\">",
      "      <IconStyle>",
      "        <color>ff0066ff</color>",
      "        <scale>0.8</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"90disconnectionsSmall\">",
      "      <IconStyle>",
      "        <color>ff0033ff</color>",
      "        <scale>0.8</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"100disconnectionsSmall\">",
      "      <IconStyle>",
      "        <color>ff0000ff</color>",
      "        <scale>0.8</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"noDisconnectionsMedium\">",
      "      <IconStyle>",
      "        <color>ff00ff00</color>",
      "        <scale>1.0</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"10disconnectionsMedium\">",
      "      <IconStyle>",
      "        <color>ff00ff33</color>",
      "        <scale>1.0</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"20disconnectionsMedium\">",
      "      <IconStyle>",
      "        <color>ff00ff66</color>",
      "        <scale>1.0</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"30disconnectionsMedium\">",
      "      <IconStyle>",
      "        <color>ff00ff99</color>",
      "        <scale>1.0</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"40disconnectionsMedium\">",
      "      <IconStyle>",
      "        <color>ff00ffcc</color>",
      "        <scale>1.0</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"50disconnectionsMedium\">",
      "      <IconStyle>",
      "        <color>ff00ffff</color>",
      "        <scale>1.0</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"60disconnectionsMedium\">",
      "      <IconStyle>",
      "        <color>ff00ccff</color>",
      "        <scale>1.0</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"70disconnectionsMedium\">",
      "      <IconStyle>",
      "        <color>ff0099ff</color>",
      "        <scale>1.0</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"80disconnectionsMedium\">",
      "      <IconStyle>",
      "        <color>ff0066ff</color>",
      "        <scale>1.0</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"90disconnectionsMedium\">",
      "      <IconStyle>",
      "        <color>ff0033ff</color>",
      "        <scale>1.0</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"100disconnectionsMedium\">",
      "      <IconStyle>",
      "        <color>ff0000ff</color>",
      "        <scale>1.0</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"noDisconnectionsBig\">",
      "      <IconStyle>",
      "        <color>ff00ff00</color>",
      "        <scale>1.2</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"10disconnectionsBig\">",
      "      <IconStyle>",
      "        <color>ff00ff33</color>",
      "        <scale>1.2</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"20disconnectionsBig\">",
      "      <IconStyle>",
      "        <color>ff00ff66</color>",
      "        <scale>1.2</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"30disconnectionsBig\">",
      "      <IconStyle>",
      "        <color>ff00ff99</color>",
      "        <scale>1.2</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"40disconnectionsBig\">",
      "      <IconStyle>",
      "        <color>ff00ffcc</color>",
      "        <scale>1.2</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"50disconnectionsBig\">",
      "      <IconStyle>",
      "        <color>ff00ffff</color>",
      "        <scale>1.2</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"60disconnectionsBig\">",
      "      <IconStyle>",
      "        <color>ff00ccff</color>",
      "        <scale>1.2</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"70disconnectionsBig\">",
      "      <IconStyle>",
      "        <color>ff0099ff</color>",
      "        <scale>1.2</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"80disconnectionsBig\">",
      "      <IconStyle>",
      "        <color>ff0066ff</color>",
      "        <scale>1.2</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"90disconnectionsBig\">",
      "      <IconStyle>",
      "        <color>ff0033ff</color>",
      "        <scale>1.2</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"100disconnectionsBig\">",
      "      <IconStyle>",
      "        <color>ff0000ff</color>",
      "        <scale>1.2</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"noDisconnectionsHuge\">",
      "      <IconStyle>",
      "        <color>ff00ff00</color>",
      "        <scale>1.4</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"10disconnectionsHuge\">",
      "      <IconStyle>",
      "        <color>ff00ff33</color>",
      "        <scale>1.4</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"20disconnectionsHuge\">",
      "      <IconStyle>",
      "        <color>ff00ff66</color>",
      "        <scale>1.4</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"30disconnectionsHuge\">",
      "      <IconStyle>",
      "        <color>ff00ff99</color>",
      "        <scale>1.4</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"40disconnectionsHuge\">",
      "      <IconStyle>",
      "        <color>ff00ffcc</color>",
      "        <scale>1.4</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"50disconnectionsHuge\">",
      "      <IconStyle>",
      "        <color>ff00ffff</color>",
      "        <scale>1.4</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"60disconnectionsHuge\">",
      "      <IconStyle>",
      "        <color>ff00ccff</color>",
      "        <scale>1.4</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"70disconnectionsHuge\">",
      "      <IconStyle>",
      "        <color>ff0099ff</color>",
      "        <scale>1.4</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"80disconnectionsHuge\">",
      "      <IconStyle>",
      "        <color>ff0066ff</color>",
      "        <scale>1.4</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"90disconnectionsHuge\">",
      "      <IconStyle>",
      "        <color>ff0033ff</color>",
      "        <scale>1.4</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>",
      "    <Style id=\"100disconnectionsHuge\">",
      "      <IconStyle>",
      "        <color>ff0000ff</color>",
      "        <scale>1.4</scale>",
      "        <Icon>",
      "          <href>http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>",
      "        </Icon>",
      "      </IconStyle>",
      "    </Style>"
    ),
    collapse = "\n"
  )

  # Create KML for event location.
  kml_event_location <- kml_snippet("Event location", event_longitude, event_latitude, "#eventLocation")

  # Create KML for each postcode point. Postcodes are grouped by disconnection percentages.
  grouped_circuit_data <- group_by_percentage_disconnect(map_data, scaling)
  grouped_circuit_data_list <- split(grouped_circuit_data, grouped_circuit_data$percentage_bins)
  kml_circuits <- kml_for_grouped_circuits(grouped_circuit_data_list)

  # Add concentric circles.
  kml_circles <- paste(
    kml_circle("zone 1", event_longitude, event_latitude, zone_one_radius),
    kml_circle("zone 2", event_longitude, event_latitude, zone_two_radius),
    kml_circle("zone 3", event_longitude, event_latitude, zone_three_radius),
    sep = "\n"
  )

  # Each KML has to end with the following footer.
  kml_footer <- "  </Document>\n</kml>"
  kml_output <- paste(c(kml_header, kml_circuits, kml_circles, kml_event_location, kml_footer), collapse = "\n")
  return(kml_output)
}

#' From each row within the map_data row, create KML snippet
#' using its postcode, percentage of disconnections, number of sites, longitude and latitude.
kml_snippet_from_row <- function(row, scaling) {
  name <- paste0("Postcode ", row[["s_postcode"]])
  # Change colour depending on percentage of disconnections.
  # FIXME: Change colour to go from blue to red instead of orange.
  if (row[["num_disconnects"]] == 0) {
    urlStylePercentages <- "#noDisconnections"
  } else if (row[["num_disconnects"]] == row[["system_count"]]) {
    urlStylePercentages <- "#100disconnections"
  } else {
    # Change the colour depending on the percentage of disconnections.
    rounded_percentage <- (floor(as.double(row[["percentage_disconnect"]]) * 10) + 1) * 10
    urlStylePercentages <- paste0("#", rounded_percentage, "disconnections")
  }
  # Change size depending on number of circuits.
  if (isTRUE(scaling)) {
    if (as.numeric(row[["system_count"]]) < 3) {
      urlStyleSize <- "Tiny"
    } else if (as.numeric(row[["system_count"]]) < 6) {
      urlStyleSize <- "Small"
    } else if (as.numeric(row[["system_count"]]) < 10) {
      urlStyleSize <- "Medium"
    } else if (as.numeric(row[["system_count"]]) < 15) {
      urlStyleSize <- "Big"
    } else {
      urlStyleSize <- "Huge"
    }
  } else {
    urlStyleSize <- "Medium"
  }
  styleUrl <- paste0(urlStylePercentages, urlStyleSize)
  description <- paste0(
    "Postcode: ",
    row[["s_postcode"]],
    "\nPercentage disconnect: ",
    row[["percentage_disconnect"]],
    "\nNumber of sites: ",
    row[["system_count"]]
  )
  kml <- kml_snippet("", row[["lon"]], row[["lat"]], styleUrl, description)
  return(kml)
}

#' Create KML snippet from name, longitude, latitude, styleUrl and description.
kml_snippet <- function(name, lon, lat, styleUrl, description = "") {
  snippet <- paste(
    c(
      "      <Placemark>",
      paste0("        <name>", name, "</name>"),
      paste0("        <description>", description, "</description>"),
      paste0("        <styleUrl>", styleUrl, "</styleUrl>"),
      "        <Point>",
      paste0("          <coordinates>", lon, ",", lat, "</coordinates>"),
      "        </Point>",
      "      </Placemark>"
    ),
    collapse = "\n"
  )
  return(snippet)
}

#' Create KML snippet for a circle with longitude, latitude and radius provided.
kml_circle <- function(name, longitude, latitude, radius, colour = "#ffaaaaaa", description = "", width = 2) {
  kml_coordinates <- generate_circle_coordinates(longitude, latitude, radius)
  circle_kml_snippet <- paste(
    c(
      "    <Placemark>",
      paste0("      <name>", name, "</name>"),
      paste0("      <description>", description, "</description>"),
      "      <Style>",
      "        <IconStyle>",
      "          <Icon/>",
      "        </IconStyle>",
      "        <LineStyle>",
      paste0("          <color>", colour, "</color>"),
      paste0("          <width>", width, "</width>"),
      "        </LineStyle>",
      "      </Style>",
      "      <LineString>",
      "        <tessellate>1</tessellate>",
      paste0("        <coordinates>", kml_coordinates, "</coordinates>"),
      "      </LineString>",
      "    </Placemark>"
    ),
    collapse =  "\n"
  )
  return(circle_kml_snippet)
}

#' Helper function for generating KML circle coordinates. Stitches together the longitude and latitude with a comma.
coord_to_string <- function(row) {
  return(paste0(row[["lon"]], ",", row[["lat"]]))
}

#' Helper function for generating KML circle coordinates.
generate_circle_coordinates <- function(longitude, latitude, radius) {
  coordinates <- circle.polygon(
    longitude,
    latitude,
    radius,
    sides = 100,
    units = "km",
    poly.type = "gc.earth",
    by.length = FALSE
  )
  coordinates <- paste(apply(coordinates, 1, coord_to_string), collapse = " ")
  return(coordinates)
}

#' Takes a number between 0 and 1 and returns 0, 10, 20, ..., 100 depending on in which percentage bin it is in.
#' e.g.    0 ->   0%
#'      0.06 ->  10%
#'      0.61 ->  70%
#'      0.95 -> 100%
calculate_percentage_bin <- function(number) {
  return(ceiling(as.double(number) * 10) * 10)
}

#' Group map data by percentage disconnects (separated into 0%, 10%, 20%, ..., 100% frequency bins).
#' Also creates the column kml_snippet.
group_by_percentage_disconnect <- function(map_data, scaling = TRUE) {
  map_data <- map_data %>%
    mutate(percentage_bins = calculate_percentage_bin(percentage_disconnect)) %>%
    rowwise %>%
    do({
        result <- as_tibble(.)
        result$kml_snippet <- kml_snippet_from_row(result, scaling)
        result
    }) %>%
    group_by(percentage_bins)
  return(map_data)
}

#' Create KML for groups of circuits. Create a folder for each 10% percentage disconnect interval,
#' i.e. no disconnects, 10% disconnects, 20% disconnects, ..., 90% disconnects, 100% disconnects.
#' By putting each group into its own folder, we can easily hide/show the group in Google Earth.
kml_for_grouped_circuits <- function(grouped_circuit_data) {
  kml <- ""
  for (name in names(grouped_circuit_data)) {
    if (kml == "")  {
      kml <- paste(
        c(
          "    <Folder>",
          paste0("      <name>", name, "% disconnections</name>"),
          paste(grouped_circuit_data[[name]]$kml_snippet, collapse = "\n"),
          "    </Folder>"
        ),
        collapse = "\n"
      )
    } else {
      kml <- paste(
        c(
          kml,
          "    <Folder>",
          paste0("      <name>", name, "% disconnections</name>"),
          paste(grouped_circuit_data[[name]]$kml_snippet, collapse = "\n"),
          "    </Folder>"
        ),
        collapse = "\n"
      )
    }
  }
  return(kml)
}
