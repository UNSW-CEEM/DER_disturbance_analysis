documentation_panel <- function(){
  panel <- mainPanel(
    h3('Categorisation of sites and circuits'),
    h4('Inverter Standard Definition'),
    div('Solar Analytics pv and inverter circuits are assigned an inverter standard based on the install date
        provided in the corresponding site details record. If there are two or more records for any given site then
        the install date provided in the first record is used. Installation dates before 2015-10-01 are asigned to
        the AS4777.3:2005 standard, dates on or after 2015-10-01 and before 2016-11-01 are asigned to the Transition 
        standard and dates on or after the 2016-11-01 are asigned to the AS4777.2:2015 standard. Where only a 
        month and year are given for the installation date then an installation day of the 28th of the month is 
        assumed. Where no date is given or the date cannot be converted to a datetime object then a date of 
        2015-11-28 is assumed, placing the date in the Transition standard.'),
    h4('Response Category Definition'),
    div('Circuits can be assigned one of 8 response types, 4 of which represent actual response types and 3 which
        provided a reason a circuit could not be categorised. The actual response categories are:'),
    tags$ul(tags$li('"Ride Through" which is assigned to circuits that at most drop their output by 4 % during the 
                      event window specified by the user'), 
            tags$li('"Curtail" which is asigned to circuits that drop their output by more than 4 % but no more
                    than 95 %'),
            tags$li('"Drop to Zero" which is assigned to circuits that drop their output by more than 95 % for one
                    interval during the event window'),
            tags$li('"Disconnect" which is asigned to circuits that drop their output by more than 95 % for a least
                    2 intervals')), 
    div('The remaining 4 categories are'),
    tags$ul(tags$li('"Off at t0" which is assigned to circuits that have an output of less than 0.1 kW at the 
                    start time of the event window'),
            tags$li('"Not enough data" which is assigned to circuits that have missing data points during the 
                    event window and'),
            tags$li('"NA" which is assigned to circuits that have zero data points during the event window'),
            tags$li('"Undefined" is the default category for circuits that do not fall into one of the above categories,
                    circuits falling into this category probably indicates the tool is not functioning properly')),
    h4('Zone Category Definition'),
    div('Circuits can be assigned one of 5 zone categories:'), 
    tags$ul(tags$li('zones 1 to 3 are assigned to circuits between the outer radius of zone and the outer radius 
                  of the lower zone'),
            tags$li('The "Undefined" zone is assigned to circuits outside the outer radius of zone 3'),
            tags$li('The "NA" zone is assigned to circuits with no location data, i.e. no postcode')),
    div('Note circuit locations are based on the centroid their site\'s postcode.'),
    h4('Compliance Status Category Definition'),
    div('Compliance status is assigned on a site basis, currently it pertains only to a site\'s aggregate over 
        frequency droop response. A mean error metric, max error metric and min error metric are 
        calculated for each site, i.e. it assumes sites are responing only to an over frequency event. 
        Where the mean error metric is defined as per slide 1. The max and min error
        metrics are calculated similarly. Additionally a threshold error metric is calculated as the mean error metric 
        of a "flat line" response (see slide 2). Based on these metric sites are then categorised as follows:'),
    tags$ul(tags$li('"Compliant" which is asigned to sites where the max error metric is less than or equal to zero,
                      i.e. the response of the site was always less than or equal to the ideal response'),
            tags$li('"Ambigous" which is asigned to sites where the max error metric is equal to or greater than zero 
                      and the min error metric is equal to or less than zero'),
            tags$li('"Above Ideal Response" which is asigned to sites where the min error metric is equal to or
                      greater than zero and the mean error metric is less than the threshold error metric'),
            tags$li('"Non Compliant" which is assigned to sites where the min error metric is greater than or
                    equal to zero and the mean error metric is greater than or equal to the threshold error metric'),
            tags$li('"NA" which is assigned to sites where there is no data available during the Ideal Response peroid'),
            tags$li('"Undefined" is the default category for sites that do not fall into one of the above categories, 
                    sites should only fall into this category if there is no Ideal Response for the given frequency 
                    data set.')),
    img(src='slide1.png'),
    img(src='slide2.png'),
    h3('Further Methodology Notes on a Chart Basis'),
    h4('Aggregate Power Chart'),
    div('By default this chart shows aggregate power of the Solar Analytics data on a basis determined by the grouping
        variables chosen by the user. If no grouping variables are chosen the data is grouped on a cleaned/raw basis.
        If the additional processing of "Upscaling" is selected then the aggregate power is upscaled to estimate the output
        of the population of systems. Note upscaling is only possible with grouping variables "AS4777" and 
        "Size Grouping". The upscaling methodology is as follows:'),
    tags$ul(tags$li('The installed capacity of systems at the time of the vent for each combination of the grouping 
                     variables is determined from the data file "cumulative_capacity_and_number_20190121"'),
            tags$li('For each time interval where output data is available the performance factor of each site is determined, 
                     as the: site power / site capacity. This is done to levelise the effect of large and small systems
                    on the upscaling'),
            tags$li('For each time interval and combination of grouping variables the mean performance factor is found. Note 
                    as finding the mean ignores missing data this method implicity ignores missing data from system rather
                    than interpreting it as zero value'),
            tags$li('Then the mean peformance factor for each  time interval and combination of grouping variables is 
                     mutipllied by the corresponding intalled capacity number to the upscaled aggregate power.')),
    h4('Event Normalised Chart'),
    div('By default this chart shows event normalised site performance factors on a basis determined by the grouping 
         variables chosen by the user. If no grouping variables are chosen the data is grouped on a cleaned/raw basis.
        The calculation of these values procceds in the following way:'),
    tags$ul(tags$li('For each time interval and combination of grouping variables the mean site performance factor is 
                    found, as in the upscaling method'),
            tags$li('The mean peformance factor at the time of the users specified pre-event interval is found'),
            tags$li('Then the mean peformance factor for each interval is divided by the corresponding pre-event mean
                    performance factor, to give the time series of pre-event normalised mean peformance factors on
                    an aggregate basis.')),
    div('Note if the event normalised power is equal to or less than 0.00001, then the re-event normalised mean 
        peformance factor is given a value of NA, to avoid the effects of divition by near zero numbers.'),
    h4('Frequency Chart'),
    div('By default this chart shows the mean frequency reported by Solar Analytics devices on a basis determined by the grouping 
         variables chosen by the user. It also shows the regional high speed frequency data if available.'),
    h4('Voltage Chart'),
    div('By default this chart shows the mean voltage reported by Solar Analytics devices on a basis determined by the grouping 
         variables chosen by the user.'),
    h4('Circuit Responses Chart'),
    div('This chart shows the percentage of circuits in each response category, the percentage is based on the number 
        circuits remaining post any filtering specified by the user. Break down to further sub categories by color is
        based on the user specified grouping variables.'),
    h4('Distance Response Chart'),
    div('This chart shows the distance response of circuits, where the distance response is defined as, the cumulative 
        number circuts that have disconnected divided by the cumulative number of systems, where the cumulative is on
        distance from the event location basis. For the purposes of this graph disconnected circuits are count as those
        in response categories 3 and 4. As with other geospatial calculations in the tool, circuit locations are taken
        as the postcode centroids. Note the following grouping variables (that are heaviliy correlated with location) 
        are ignored for this chart, Zone, Postcode and Circuit.'),
    h4('Circuit Responses Chart'),
    div('This chart shows the  break down of circuits responses by zone, the percentage is based on the number 
        circuits remaining post filtering in each zone. Break down to further sub categories by color is
        based on the user specified grouping variables. Note the grouping variable circuits is ignored for this chart.'),
    h3('Data processing and cleaning')
    )
  return(panel)
  }