documentation_panel <- function(){
  panel <- mainPanel(
    h3('Categorisation of sites and circuits'),
    h4('Inverter standard definition'),
    div('Circuits are assigned an inverter standard based on the install date
        provided in the corresponding site details record. If there are two or more records for any given site then
        the install date provided in the first record is used. Installation dates before 2015-10-01 are asigned to
        the AS4777.3:2005 standard, dates on or after 2015-10-01 and before 2016-11-01 are asigned to the Transition 
        standard and dates on or after the 2016-11-01 are asigned to the AS4777.2:2015 standard. Where only a 
        month and year are given for the installation date then an installation day of the 28th of the month is 
        assumed. Where no date is given or the date cannot be converted to a datetime object then a date of 
        2015-11-28 is assumed, placing the date in the Transition standard.'),
    h4('Response category definition'),
    div('Circuits can be assigned one of 8 response types, 4 of which represent actual response types and 4 which
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
    h4('Zone category definition'),
    div('Circuits can be assigned one of 5 zone categories:'), 
    tags$ul(tags$li('zones 1 to 3 are assigned to circuits between the outer radius of zone and the outer radius 
                  of the lower zone'),
            tags$li('The "Undefined" zone is assigned to circuits outside the outer radius of zone 3'),
            tags$li('The "NA" zone is assigned to circuits with no location data, i.e. no postcode')),
    div('Note circuit locations are based on the centroid of their site\'s postcode.'),
    h4('Compliance status category definition'),
    div('Compliance status is assigned on a site basis (because we only know the applicable standard on a site basis), 
        currently it pertains only to a site\'s aggregate over 
        frequency droop response, i.e. it assumes sites are responing only to an over frequency event. 
        An error metric, max percentage error and min percentage error are calculated for each site. 
        The error metric is defined as per slide 1, the min percentage error and max percentage error
        are calculated similarly. Additionally a threshold error metric is calculated as the error metric 
        of a "flat line" response (see slide 2). Based on these metric, sites are then categorised as follows:'),
    tags$ul(tags$li('"Compliant" which is asigned to sites where the max percentage error is less than or equal to zero,
                      i.e. the response of the site was always less than or equal to the ideal response'),
            tags$li('"Ambigous" which is asigned to sites where the max percentage error is equal to or greater than zero 
                      and the min percentage error is equal to or less than zero'),
            tags$li('"Above Ideal Response" which is asigned to sites where the min percentage error is equal to or
                      greater than zero and the error metric is less than the threshold error metric'),
            tags$li('"Non Compliant" which is assigned to sites where the min percentage error is greater than or
                    equal to zero and the error metric is greater than or equal to the threshold error metric'),
            tags$li('"NA" which is assigned to sites where there is no data available during the Ideal Response peroid'),
            tags$li('"Undefined" is the default category for sites that do not fall into one of the above categories, 
                    sites should only fall into this category if there is no Ideal Response for the given frequency 
                    data set.')),
    img(src='slide1.png'),
    img(src='slide2.png'),
    h3('Further methodology notes on a chart basis'),
    h4('Aggregate power chart'),
    div('By default this chart shows the aggregate power on a basis determined by the grouping
        variables chosen by the user. If no grouping variables are chosen the data is grouped on a cleaned/raw basis.
        If the additional processing of "Upscaling" is selected then the aggregate power is upscaled to estimate the output
        of the population of systems. Note upscaling is only possible with grouping variables "AS4777" and 
        "Size Grouping". The upscaling methodology is as follows:'),
    tags$ul(tags$li('The installed capacity of systems at the time of the event for each combination of the grouping 
                     variables is determined from the data file "cumulative_capacity_and_number_20190121"'),
            tags$li('For each time interval where output data is available the performance factor of each site is determined, 
                     as the: site power / site capacity. This is done to levelise the effect of large and small systems
                    on the upscaling'),
            tags$li('For each time interval and combination of grouping variables the mean performance factor is found. Note 
                    as finding the mean ignores missing data this method implicity ignores missing data from systems rather
                    than interpreting it as zero value'),
            tags$li('Then the mean peformance factor for each  time interval and combination of grouping variables is 
                     multiplied by the corresponding intalled capacity number to find the upscaled aggregate power.')),
    h4('Event normalised chart'),
    div('By default this chart shows event normalised site performance factors on a basis determined by the grouping 
         variables chosen by the user. If no grouping variables are chosen the data is grouped on a cleaned/raw basis.
        The calculation of these values procceds in the following way:'),
    tags$ul(tags$li('For each time interval and combination of grouping variables the mean site performance factor is 
                    found, as in the upscaling method'),
            tags$li('The mean peformance factor at the time of the users specified pre-event interval is found'),
            tags$li('Then the mean peformance factor for each interval is divided by the corresponding pre-event mean
                    performance factor, to give the time series of pre-event normalised mean peformance factors on
                    an aggregate basis.')),
    div('Note if the event normalised power is equal to or less than 0.00001, then the pre-event normalised mean 
        peformance factor is given a value of NA, to avoid the effects of divition by near zero numbers.'),
    h4('Frequency chart'),
    div('By default this chart shows the mean frequency reported by Solar Analytics devices on a basis determined by the grouping 
         variables chosen by the user. It also shows the regional high speed frequency data if available.'),
    h4('Voltage chart'),
    div('By default this chart shows the mean voltage reported by Solar Analytics devices on a basis determined by the grouping 
         variables chosen by the user.'),
    h4('Circuit responses chart'),
    div('This chart shows the percentage of circuits in each response category, the percentage is based on the number 
        circuits remaining post any filtering specified by the user. The breakdown to further sub categories by color is
        based on the user specified grouping variables.'),
    h4('Distance response chart'),
    div('This chart shows the distance response of circuits, where the distance response is defined as, the cumulative 
        number circuts that have disconnected divided by the cumulative number of systems, where the cumulative is on
        the distance from the event location. For the purposes of this graph disconnected circuits are counted as those
        in response categories 3 and 4. As with other geospatial calculations in the tool, circuit locations are taken
        as the postcode centroids. Note the following grouping variables (that are heaviliy correlated with location) 
        are ignored for this chart, Zone, Postcode and Circuit.'),
    h4('Zone responses chart'),
    div('This chart shows the  break down of circuits responses by zone, the percentage is based on the number 
        circuits remaining, post filtering, in each zone. Break down to further sub categories by color is
        based on the user specified grouping variables. Note the grouping variable Circuits is ignored for this chart.'),
    h4('Geospatial response chart'),
    div('This chart shows the percentage of systems disconnecting in each postcode on a geospatial basis, with a dot
        representing each postcode located at the postcodes centroid. Note for the purposes of this chart disconnected 
        systems are counted as those in categories 3 and 4. User chosen grouping variables do not effect this chart. 
        Cleaned data is dispalyed if available else raw data is used. Be aware that the density of dots does not 
        represent the density of pv systems, rather it represents the density of postcodes.'),
    h4('Circuit count table'),
    div('This table shows the number of circuits in each combination of the user specified grouping variables. Note for
        this table if the grouping varaible Circuits is selected then the number of circuts by site is displayed.'),
    h3('Data processing and cleaning'),
    h4('Determination of timeseries duration'),
    div('The duration (or frequency) of the timseries data for each circuit is calculated independently. This is done
        by finding the time step between each record in the circuit\'s timeseries data. Then the mode and min of the 
        set of time steps is found, where the mode time step is equal to the min time step, the circuit is assigned this time step 
        as its nominal duration, otherwise is assigned a duration of NA and will be filtered out of the data used by the
        tool.'),
    h4('Determination of timeseries offset'),
    div('The timeseries offset for each circuit is calculated independently. This is done by finding the set of seconds
        that a circuit reports on, i.e. for a circuit with a duration 60 s this may just be 55, or for a circuit with a 
        duration of 5 s this maybe 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 55. The timeseries offset is taken as the 
        minumim from this set that occurs atleast 100 times in the data set. If no offset occurs atleast 100 times than
        the offset is defined as NA. By default the tool shows just the dominate time offset group but this behaviour
        can be ajusted by the user.'),
    h4('Data cleaning tab overview'),
    div('If the data cleaning function has been turned on before loading data, then the data cleaning tab will display 
        two tables, the first table is the site details dataset post cleaning and the second table is the circuit details 
        dataset post cleaning. Selecting a row in either table will plot the corresponding timeseries data at the top of 
        the page. Flags in both tables indicate where values have been changed, the old values are also recorded in the table,
        sorting by the change flags should enable the user to quickly review what changes that have been made. Double clickling
        in a cell enables the user to edit values. Saving the data using the button at the bottom of page does two
        things, it saves a copy of both tables as CSVs, with their orginal names and the suffix "_cleaned", it also
        overwites the "cleaned" dataset in the tool with any manual changes the user has made. Note loading saved versions
        of these table in the tool at a later date should be possible but they will be treated as "raw" data. Additionally
        if the user wants to manually exclude circuits they should edit the con_type, adding the suffix "_exclude".'),
    h4('AC and DC capacity auto cleaning'),
    div('The following checks and corrections are peformed as part of the site details data cleaning process, they are 
        peformed in the order shown:'),
    tags$ul(tags$li('Checking that the AC capcaity has not been provided in watts, note that the AC capacity should be
                    in kW and the DC capacity should be in watts. If the AC capacity divided by the dc is greater than 
                    0.1 and the AC capacity is greater than 150, then assume the AC capacity was provided in watts rather
                    than kW and divide the provided value by 1000'),
            tags$li('Checking that the peak site power is feasable given the DC capacity, and scaling up the DC
                    if it is not. If the DC power divide by the maximum power is less than 0.9, then scale up the DC
                    capacity by the smallest integer value that will correct this. This based on the assumption that the 
                    likely reason for a small DC capacity value is that only the value of one of several DC arrays has 
                    been recorded, note if the reason was something else like the DC capacity being reported in watts 
                    rather than kW, then this method till provides an improved estimate of the DC capacity.'),
            tags$li('Checking that the peak site power is feasable given the AC capacity, and scaling up the AC
                    if it is not. If the AC capacity divided by the maximum power is less than 0.6 and DC capacity was 
                    previously scaled, then scale the AC capacity by the smallest integer number that makes it larger 
                    than the DC capacity. If the AC capacity divided by the maximum power is less than 0.6 and DC capacity 
                    was not previously scaled, then scale the AC capacity by the smallest integer number that makes it larger 
                    than the site maximum power.')
            ),
    h4('Connection type cleaning'),
    div('Checking if circuits recorded as a PV type have energy profiles that match this type. This is done by using 
        the sites postcode and the date to find the sunrise and sunset time. If 25 % pecent or more of the energy was
        recorded outside daylight hours (plus a one hour buffer each way). And the site recorded more energy then the
        equavlent of operating at a 1 % capacity factor for 24 hours (using the first site AC capacity value). Then if
        the site had a PV connection type change this to type "load".'),
    h4('Polarity cleaning'),
    div('There are two types of polarity check peformed, these are done in the order shown:'),
    tags$ul(tags$li('Checking if the site specified polarity is not reversed. If the magnitude of the site\'s minimum power is 
                    greater than the magnitude of the site\'s maximum power. And the site recorded more energy then the
                    equavlent of operating at a 1 % capacity factor for 24 hours (using the site\'s first AC capacity value). 
                    Then if the site had a PV connection type, reverse it ploarity.'),
            tags$li('Checking if the site specified polarity is definitive. If the site\'s maximum power is 
                    greater than 10 % of the site\'s first AC capacity. And the site\'s minimum power is 
                    greater than 10 % of the site\'s first AC capacity times by negative one. Then change the site con_type
                    to "mixed".')
            )
    )
  return(panel)
  }