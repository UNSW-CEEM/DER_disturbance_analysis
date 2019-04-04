documentation_panel <- function(){
  panel <- mainPanel(
    h3('Categorisation of sites and circuits'),
    h4('Inverter Standard Definition'),
    div('Solar Analytics pv and inverter circuits are assigned an inverter standard based on the install date
        provided in the corresponding site details record. If there are two records for any given site then
        the install date provided in the first record is used. Installation dates before 2015-10-01 are asigned to
        the AS4777.3:2005 standard, dates on or after 2015-10-01 and before 2016-11-01 are asigned to the Transition 
        standard and dates on or after the 2016-11-01 are asigned to the AS4777.2:2015 standard. Where only a 
        month and year are given for the installation date then an installation day of the 28th of the month is 
        assumed. Where no date is given or the date cannot be converted to a datetime object then a date of 
        2015-11-28 is assumed, placing the date in the Transition standard.'),
    h4('Response Category Definition'),
    div('Circuits can be assigned one of 7 response types, 4 of which represent actual response types and 3 which
        provided a reason a circuit could not be categorised. The actual response categories are "Ride Through"
        which is assigned to circuits that at most drop their output by 4 % during the event window specified by
        the user, "Curtail" which is asigned to circuits that drop their output by more than 4 % but no more
        than 95 %, "Drop to Zero" which is assigned to circuits that drop their output by more than 95 % for one
        interval during the event window, "Disconnect" whcih is asigned to circuits that drop their output by more
        than 95 % for a least 2 intervals. The remaining 3 categories are "Off at t0" which 
        is assigned to circuits that have an output of less than 0.1 kW at the start time of the event window, 
        "Not enough data" which is assigned to circuits that have missing data points during the event window and 
        "NA" which is assigned to circuits that have zero data points during the event window.'),
  h4('Zone Category Definition'),
  div('Circuits can be assigned one of 5 zone categories, zones 1 to 3 are assigned to circuits between the outer radius 
      of zone and the outer radius of the lower zone. The "undefined" zone is assigned to circuits outside the outer
      radius of zone 3. The "NA" zone is assigned to circuits with no location data, i.e. no postcode. Note circuit 
      locations are based on the centroid their site\'s postcode.'))
  return(panel)
  }