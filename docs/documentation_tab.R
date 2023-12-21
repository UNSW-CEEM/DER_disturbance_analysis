documentation_panel <- function() {
  panel <- mainPanel(
    h3("Categorisation of sites and circuits"),
    h4("Inverter standard definition"),
    div(
      "Circuits are assigned an inverter standard based on the install date provided in the corresponding site details
      record. If there are two or more records for any given site then the install date provided in the first record is
      used. Installation dates before 2015-10-01 are assigned to the AS4777.3:2005 standard, dates on or after
      2015-10-01 and before 2016-11-01 are assigned to the Transition standard. Dates on or after the 2016-11-01 and
      before 2020-12-01 are assigned to the AS4777.2:2015 standard, except for South Australia where dates on or after
      2020-10-01 are assigned to AS4777.2:2015 VDRT. Dates on or after 2020-12-01 and before 2022-01-01 are assigned to
      AS4777.2:2020. This means that only a small fraction of dates will be assigned to AS4777.2:2015 VDRT (i.e. install
      dates for South Australian systems during 2020-10 and 2020-11). Dates where only a month and year are given for
      the installation date then an installation day of the 28th of the month is assumed. Where no date is given or the
      date cannot be converted to a datetime object then a date of 2015-11-28 is assumed, placing the date in the
      Transition standard."
    ),
    h4("Response category definition"),
    div(
      "Circuits can be assigned one of 8 response types, 4 of which represent actual response types and 4 which
      provide a reason for why a circuit could not be categorised. The actual response categories are:"
    ),
    tags$ul(
      tags$li(
        '"Ride Through" which is assigned to circuits that drop their output by at most 4 % during the event window
        specified by the user;'
      ),
      tags$li(
        '"Curtail" which is assigned to circuits that drop their output by more than 4 % but no more than 95 %;'
      ),
      tags$li(
        '"Drop to Zero" which is assigned to circuits that drop their output by more than 95 % for one interval
        during the event window;'
      ),
      tags$li(
        '"Disconnect" which is assigned to circuits that drop their output by more than 95 % for at least 2 intervals.'
      )
    ),
    div("The remaining 4 categories are:"),
    tags$ul(
      tags$li(
        '"Off at t0" which is assigned to circuits that have an output of less than 0.1 kW at the start time of the
        event window;'
      ),
      tags$li(
        '"Not enough data" which is assigned to circuits that have missing data points during the event window;'
      ),
      tags$li('"NA" which is assigned to circuits that have zero data points during the event window;'),
      tags$li(
        '"Undefined" is the default category for circuits that do not fall into one of the above categories: circuits
        falling into this category probably indicates that the tool is not functioning properly.'
      )
    ),
    h4("UFLS classification"),
    div(
      "Circuits which disconnect due to UFLS are identified by two methods. The first is intended for datasets that have
      missing data in the case of a UFLS dropout and involves a timestamp check which looks at the number of timesteps
      included in a customisable window length directly before and after the event. If the circuit has greater than
      the threshold samples before and 0 samples after, it is classified as UFLS Dropout. The second method was
      developed for the Tesla data in which a site can island and so timesteps may not be skipped. This method
      calculates the average voltage at the site for a set window size before and after the event time, and a drop from
      a pre-event average > 180 to post event average < 180 is classified as UFLS Dropout. If a circuit is classified
      as dropout by either of these tests, then its response category is changed to UFLS Dropout."
    ),
    h4("Islanded site assessment"),
    div(
      "Sites with PV and battery may island from the grid due to the action of the battery inverter/​gateway rather than
      the PV inverter. The columns 'Islanded', 'island_assessment', and 'islanding_alert' in the circuit summary are
      created based on a provided alerts file and additional analysis of the voltage and frequency signals during the
      window after the event."
    ),
    h4("Zone category definition"),
    div("Circuits can be assigned one of 5 zone categories:"),
    tags$ul(
      tags$li(
        "Zones 1 to 3 designate circuits within a certain distance of the site of the event (Zone 1 is closest,
        Zone 3 is furthest away);"
      ),
      tags$li('The "Undefined" zone is assigned to circuits outside the outer radius of Zone 3;'),
      tags$li('The "NA" zone is assigned to circuits with no location data, i.e. no postcode.')
    ),
    div("Note circuit locations are based on the centroid of their site's postcode."),
    h4("Droop response compliance status category definition"),
    div(
      'Compliance status is assigned on a circuit basis according to the following definitions. The categories
      "Disconnect", "Off at t0" and "Not enough data" are taken from the output of the response categorisation
      algorithm.'
    ),
    tags$ul(
      tags$li(
        '"Compliant": A fast and sustained reduction in output. The system reduces power by at least 50 % of the ideal
        reduction for the whole response period excluding the first minute and last minute. These parameters are
        editable in the settings tab using the compliance threshold to change the percentage, start buffer to change
        the time excluded at the start, and the end buffer to change the time excluded at the end.'
      ),
      tags$li(
        '"Non-compliant Responding": A fast but unsustained reduction in output. The system reduces power by at least
        50 % of the ideal reduction for at least one measurement interval in the first two minutes. The percentage can
        be set by changing the compliance threshold and the window to respond can be changed using the response time.'
      ),
      tags$li(
        '"Non-compliant": Neither a fast nor sustained reduction in output. The system does not fit into the "Compliant"
        nor "Non-compliant Responding" categories.'
      ),
      tags$li(
        '"Disconnect": The system dropped to zero at some stage in the response period. The system had a pre-event
        normalised power level less than 5% for at least two measurement intervals.'
      ),
      tags$li(
        '"Drop to Zero": The system dropped to zero at some stage in the response period. The system had a pre-event
        normalised power level less than 5% for at least one measurement interval.'
      ),
      tags$li(
        '"Off at t0": The systems initial power level is too low to categorise. The system had a pre-event power level
        of less than 100 W.'
      ),
      tags$li('"NA": Sites with no data available during the Ideal Response period.'),
      tags$li(
        '"Undefined": The default category for sites that do not fall into one of the above categories, sites should
        only fall into this category if there is no Ideal Response for the given frequency data set.'
      )
    ),
    h4("Reconnection compliance status category definition"),
    div("A compliance status is assigned on a circuit basis according to the following steps."),
    tags$ol(
      tags$li(
        "The resource limited interval is defined as the first interval during the reconnection period where the ramp
        rate drops by 10 % absolute."
      ),
      tags$li(
        "The total ramp is calculated from power profiles that exceed the ramp rate threshold (default 33% per min).
        Only ramps occurring before the resource limited interval are included."
      ),
      tags$li("Categorised as compliant if the total ramp greater than the threshold is less than 12.5% (default)."),
      tags$li(
        "Categorised as non-compliant if the total ramp greater than the threshold is greater than 25.0% (default)."
      ),
      tags$li("Categorised as unsure if not compliant or non-compliant."),
      tags$li(
        "Categorised as NA if the circuit does not disconnect/drop to zero during the user specified event window."
      )
    ),
    h4("Minor discrepancies in the definition of 'Reconnection compliance status' "),
    div(
      "Minor discrepancies were observed in the algorithm used for the definition of 'Reconnection compliance status'.
       Refining the algorithm is anticipated to alleviate these discrepancies."
    ),
    h3("Further methodology notes on a chart basis"),
    h4("Aggregate power chart"),
    div(
      'By default, this chart shows the aggregate power on a basis determined by the grouping variables chosen by the
      user. If no grouping variables are chosen, the data is grouped on a cleaned/raw basis. If the additional
      processing of "Upscaling" is selected, then the aggregate power is upscaled to estimate the output of the
      population of systems. Note upscaling is only possible with grouping variable "AS4777". The upscaling methodology
      is as follows:'
    ),
    tags$ul(
      tags$li(
        'The installed capacity of systems at the time of the event for each combination of the grouping
        variables is determined from the data file "cumulative_capacity_and_number.csv".'
      ),
      tags$li(
        "For each time interval where output data is available the performance factor of each site is determined,
        as the: site power/​site capacity. This is done to levelise the effect of large and small systems on the
        upscaling."
      ),
      tags$li(
        "For each time interval and combination of grouping variables, the mean performance factor is found. Note, as
        finding the mean ignores missing data, this method implicity ignores missing data from systems rather than
        interpreting it as zero value."
      ),
      tags$li(
        "Then the mean performance factor for each time interval and combination of grouping variables is multiplied by
        the corresponding installed capacity number to find the upscaled aggregate power."
      )
    ),
    h4("Event normalised power chart"),
    div(
      "By default, this chart shows event normalised circuit power on a basis determined by the grouping variables
      chosen by the user. If no grouping variables are chosen the data is grouped on a cleaned/raw basis. The
      calculation of these values proceeds in the following way:"
    ),
    tags$ul(
      tags$li(
        "The power produced by each circuit is normalised by the power produced during the pre-event time interval."
      ),
      tags$li(
        "The normalised power values of each circuit is aggregated by averaging to level of the grouping variables."
      ),
      tags$li(
        'Missing data occurs in the plot because some circuits contain NA normalised power values and these propagate
        through the aggregation. This can be solved by filtering out the "Off at t0", "Not enough data", "Undefined"
        and "NA" compliance categories.'
      )
    ),
    div(
      "Note if the event normalised power is equal to or less than 0.00001, then the pre-event normalised mean
      performance factor is given a value of NA, to avoid the effects of division by near zero numbers."
    ),
    h4("Frequency chart"),
    div(
      "By default, this chart shows the mean frequency reported by Solar Analytics devices on a basis determined by the
      grouping variables chosen by the user. It also shows the regional high speed frequency data if available."
    ),
    h4("Voltage chart"),
    div(
      "By default, this chart shows the mean voltage reported by Solar Analytics devices on a basis determined by the
      grouping variables chosen by the user."
    ),
    h4("Circuit responses chart"),
    div(
      "This chart shows the percentage of circuits in each response category. The percentage is based on the number of
      circuits remaining post any filtering specified by the user. The breakdown to further sub categories by colour is
      based on the user specified grouping variables."
    ),
    h4("Distance response chart"),
    div(
      "This chart shows the distance response of circuits, where the distance response is defined as the cumulative
      number circuits that have disconnected divided by the cumulative number of systems. For the purposes of this graph
      disconnected circuits are counted as those in response categories 3 and 4. As with other geospatial calculations
      in the tool, circuit locations are taken as the postcode centroids. Note the following grouping variables (that
      are heaviliy correlated with location) are ignored for this chart: Zone, Postcode and Circuit."
    ),
    h4("Zone responses chart"),
    div(
      "This chart shows the breakdown of circuits responses by zone. The percentage is based on the number of circuits
      remaining, post filtering, in each zone. Breakdown to further sub categories by colour is based on the user
      specified grouping variables. Note the grouping variable Circuits is ignored for this chart."
    ),
    h4("Geospatial response chart"),
    div(
      "This chart shows the percentage of systems disconnecting in each postcode on a geospatial basis, with a dot
      representing each postcode located at the postcodes centroid. Note for the purposes of this chart disconnected
      systems are counted as those in categories 3 and 4. User chosen grouping variables do not effect this chart.
      Cleaned data is dispalyed if available else raw data is used. Be aware that the density of dots does not
      represent the density of pv systems, rather it represents the density of postcodes."
    ),
    h4("Circuit count table"),
    div(
      "This table shows the number of circuits in each combination of the user specified grouping variables. Note for
      this table if the grouping variable Circuits is selected then the number of circuits by site is displayed."
    ),
    h3("Data processing and cleaning"),
    h4("Determination of timeseries duration"),
    div(
      "The duration (or frequency) of the timeseries data is given for each circuit and interval in the raw time
      series data from Solar Analytics. The duration may be 5, 30 or 60 seconds. However, sometimes this data is
      missing or has an unexpected value. For those intervals, if the actual time elapsed between the interval and the
      previous data point for the circuit is 5 seconds, then duration for that interval is replaced with 5 seconds.
      Otherwise, it is left unchanged and consequently will be filtered out from the analysis set."
    ),
    h4("Determination of timeseries offset"),
    div(
      "The timeseries offset for each circuit is calculated independently. This is done by finding the set of seconds
      that a circuit reports on, i.e. for a circuit with a duration 60 s this may just be 55, or for a circuit with a
      duration of 5 s this maybe 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 55. The timeseries offset is taken as the
      minimum from this set that occurs at least 10 times in the data set. If no offset occurs at least 10 times then
      the offset is defined as NA. By default, the tool shows just the dominant time offset group but this behaviour
      can be adjusted by the user."
    ),
    h4("Data cleaning tab overview"),
    div(
      'If the data cleaning function has been turned on before loading data, then the data cleaning tab will display
      two tables. The first table is the site details dataset post cleaning and the second table is the circuit details
      dataset post cleaning. Selecting a row in either table will plot the corresponding timeseries data at the top of
      the page. Flags in both tables indicate where values have been changed. The old values are also recorded in the
      table. Sorting by the change flags should enable the user to quickly review what changes that have been made.
      Double clicking in a cell enables the user to edit values. Saving the data using the button at the bottom of the
      page does two things: it saves a copy of both tables as CSVs, with their orginal names and the suffix "_cleaned";
      it also overwrites the "cleaned" dataset in the tool with any manual changes the user has made. Note loading saved
      versions of these tables in the tool at a later date should be possible, but they will be treated as "raw" data.
      Additionally, if the user wants to manually exclude circuits, they should edit the con_type, adding the suffix
      "_exclude".'
    ),
    h4("AC and DC capacity auto cleaning"),
    div(
      "The following checks and corrections are performed as part of the site details data cleaning process, they are
      performed in the order shown:"
    ),
    tags$ul(
      tags$li(
        "Check that the AC capacity has not been provided in watts. Note that the AC capacity should be in kW and the
        DC capacity should be in watts. If the AC capacity divided by the DC is greater than 0.1 and the AC capacity is
        greater than 150, then the tool assumes that the AC capacity was provided in watts rather than kW and divides
        the provided value by 1000."
      ),
      tags$li(
        "Check that the peak site power is feasible given the DC capacity, and scale up the DC if it is not. If the DC
        power divide by the maximum power is less than 0.9, then scale up the DC capacity by the smallest integer value
        that will correct this. This based on the assumption that the likely reason for a small DC capacity value is
        that only the value of one of several DC arrays has been recorded. Note if the reason was something else, such
        as the DC capacity being reported in watts rather than kW, then this method still provides an improved estimate
        of the DC capacity."
      ),
      tags$li(
        "Check that the peak site power is feasible given the AC capacity, and scale up the AC if it is not. If the AC
        capacity divided by the maximum power is less than 0.6 and DC capacity was previously scaled, then scale the AC
        capacity by the smallest integer number that makes it larger than the DC capacity. If the AC capacity divided by
        the maximum power is less than 0.6 and DC capacity was not previously scaled, then scale the AC capacity by the
        smallest integer number that makes it larger than the site maximum power."
      )
    ),
    h4("Connection type cleaning"),
    div(
      'Check if circuits recorded as a PV type have energy profiles that match this type. This is done by using
      the sites postcode and the date to find the sunrise and sunset time. If 25 % or more of the energy was recorded
      outside daylight hours (plus a one-hour buffer each way) and the site recorded more energy than the equivalent
      of operating at a 1 % capacity factor for 24 hours (using the first site AC capacity value) and the site had a PV
      connection type, then change this to type "load".'
    ),
    h4("Polarity cleaning"),
    div("There are two types of polarity check performed, these are done in the order shown:"),
    tags$ul(
      tags$li(
        "Check if the site specified polarity is not reversed. If the magnitude of the site's minimum power is greater
        than the magnitude of the site's maximum power and the site recorded more energy than the equivalent of
        operating at a 1 % capacity factor for 24 hours (using the site's first AC capacity value) and the site had a PV
        connection type, then reverse it polarity."
      ),
      tags$li(
        "Check if the site specified polarity is definitive. If the site's maximum power is greater than 10 % of the
        site's first AC capacity and the site's minimum power is greater than 10 % of the site's first AC capacity times
        by negative one, then change the site con_type to \"mixed\"."
      )
    ),
    h3("Manual compliance"),
    div(
      "This tab can be used to manually assign compliance categories to circuits. The functionality will become active
      after updating plots on the main tab, in order to save the manually assigned categories to the manual_compliance
      column in the circuit summary dataset, the update plots step needs to be run again on the main tab. The circuits
      are ordered randomly but with a consistent seed value, so the order will remain consistent."
    ),
    h4("Ideal Reponse Curve"),
    div("'Ideal Response Curve (Ideal reconnection profile)' illustrates changes over dutation. Potential modifications
    to the code is necessary to align the 'Ideal Response Curve' with the 'reconnection power profile'."
        )
  )
  return(panel)
}
