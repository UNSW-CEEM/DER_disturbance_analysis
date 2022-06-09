library(R6)
library(sqldf)
library(RSQLite)
library(dplyr)
library(data.table)
library(fasttime)
library(suncalc)

wd <- getwd()
setwd(dirname(parent.frame(2)$ofile))
source("data_cleaning_functions.R")
source("get_free_disk_space.R")
setwd(wd)



DBInterface <- R6::R6Class("DBInterface",
  public = list(
    db_path_name = NULL,
    default_timeseries_column_aliases = list(ts='_ts', time_stamp='_ts', c_id='_c_id', v='_voltage', vmin='_vmin', 
                                             vmax='_vmax', vmean='_vmean', f='_frequency', fmin='_fmin', fmax='_fmax',
                                             e='_e', d='_d', p='_p'),
    connect_to_new_database = function(db_path_name){
      if (file.exists(db_path_name)){
        stop("That database file already exits. If you want to create a new db with this name please delete the
             existing db. If you want to conect to an eixting db please use the connect_to_existing_database method.")
      }
      con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path_name)
      RSQLite::dbDisconnect(con)
      self$db_path_name = db_path_name
    },
    connect_to_existing_database = function(db_path_name, check_tables_have_expected_columns=TRUE){
      if (!file.exists(db_path_name)){
        stop("Specified database file not found. Please check the filepath provided.")
      }
      self$db_path_name = db_path_name
      if (check_tables_have_expected_columns) {
        self$check_tables_have_expected_columns()
      }
    },
    check_tables_have_expected_columns = function() {
      self$check_table_has_expected_columns('timeseries', c("ts", "c_id", "d", "d_key", "e", "f", "fmin", "fmax", "v", 
                                                            "vmin", "vmax", "vmean"))
      self$check_table_has_expected_columns('circuit_details_raw', c("c_id", "site_id", "con_type", "polarity", 
                                                                     "manual_droop_compliance", 
                                                                     "manual_reconnect_compliance"))
      if (self$check_if_table_exists('circuit_details_cleaned')){
        self$check_table_has_expected_columns('circuit_details_cleaned', c("c_id", "site_id", "con_type", "polarity", 
                                                                           "sunrise", "sunset", "min_power", "max_power", 
                                                                           "frac_day", "old_con_type", "con_type_changed", 
                                                                           "polarity_changed", "manual_droop_compliance", 
                                                                           "manual_reconnect_compliance"))
      }
      self$check_table_has_expected_columns('site_details_raw', c("site_id", "s_postcode", "s_state", "ac", "dc", 
                                                                  "manufacturer", "model", 
                                                                  "pv_installation_year_month"))
      
      if (self$check_if_table_exists('site_details_cleaned')){
        self$check_table_has_expected_columns('site_details_cleaned', c("site_id", "s_postcode", "s_state", 
                                                                        "pv_installation_year_month", "ac", "dc", 
                                                                        "ac_old", "dc_old", "ac_dc_ratio", "manufacturer", 
                                                                        "model", "rows_grouped", "max_power_kW", 
                                                                        "change_ac", "change_dc"))
      }
      
      if (self$check_if_table_exists('postcode_lon_lat')){
        self$check_table_has_expected_columns('postcode_lon_lat', c("postcode", "lon", "lat"))
      }
      
    },
    check_table_has_expected_columns = function(table, expected_columns){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      if (RSQLite::dbExistsTable(con, table)) {
        table <- sqldf::sqldf(paste0("select * from ", table,  " limit 1"), dbname = self$db_path_name)
        table_column_names <- sort(names(table))
        expected_columns <- sort(expected_columns)
        if (!identical(table_column_names, expected_columns)) {
          RSQLite::dbDisconnect(con)
          stop(paste0("Connection to database aborted, the tables in specified database did not have the expected columns. ",
                "This is probably becuase the database was created with a different (probably older) version of the ",
                "database interface tool. Try re-creating the database with the current version to resolve this issue."))
        }
        RSQLite::dbDisconnect(con)
      }
    },
    build_database = function(timeseries, circuit_details, site_details, alerts_file=NULL, check_disk_space=TRUE, 
                              check_dataset_ids_match=TRUE) {
      
      if (!file.exists(timeseries)){
        stop("Specified timeseries file not found. Please check the filepath provided.")
      }
      if (!file.exists(circuit_details)){
        stop("Specified circuit_details file not found. Please check the filepath provided.")
      }
      if (!file.exists(site_details)){
        stop("Specified site_details file not found. Please check the filepath provided.")
      }
      if (!is.null(alerts_file)){
        if (!file.exists(alerts_file)){
          stop("Specified alerts file not found. Please check the filepath provided.")
          } 
      }

      #free_space_gigabytes <- get_free_disk_space_in_working_directory()
      #time_series_size_gigabytes <- file.size(timeseries) / 1073741824
      #extimated_database_size <- time_series_size_gigabytes * 2.5
      #if (check_disk_space & (free_space_gigabytes - extimated_database_size) < 10.0) {
      #  stop("It is estimated that building the database will leave disk with less 
      #        than 10.0 GB of free space, build aborted as a precaution. This assumes
      #        the database is being built on the same disk as the working directory.
      #        To resolve this error please create more free space on the disk and rerun
      #        the build method or rerun the method with check_disk_space=False.")
      #}
      time_series_build_query <- self$get_time_series_build_query(timeseries)
      circuit_details_build_query <- self$get_circuit_details_build_query(circuit_details)
      site_details_build_query <- self$get_site_details_build_query(site_details)
      if (!is.null(alerts_file)){
        alerts_build_query <- self$get_alerts_build_query(alerts_file)
      }
      
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      
      # create DB history table
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS db_history")
      RSQLite::dbExecute(con, "CREATE TABLE db_history(event TEXT, utc_timestamp TEXT, tool_git_hash TEXT)")
      event <- "built"
      created_at <- lubridate::now("UTC")
      tool_git_hash <- git2r::revparse_single(revision="HEAD")$sha
      RSQLite::dbExecute(con, "INSERT INTO db_history VALUES (:event, :timestamp, :hash)",
                         params = list(event=event, timestamp=created_at, hash=tool_git_hash))

      # insert timeseries data
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS timeseries")
      
      RSQLite::dbExecute(con, "CREATE TABLE timeseries(
                         ts TEXT,
                         c_id INT,
                         d_key INT,
                         e REAL,
                         v REAL,
                         vmin REAL DEFAULT NULL,
                         vmax REAL DEFAULT NULL,
                         vmean REAL DEFAULT NULL,
                         f REAL,
                         fmin REAL DEFAULT NULL,
                         fmax REAL DEFAULT NULL,
                         PRIMARY KEY (ts, c_id, d_key))")
      
      # Suppress warning
      withCallingHandlers({
        file_con <- base::file(timeseries)
        sqldf::read.csv.sql(sql = time_series_build_query, eol = '\n',
                            dbname = self$db_path_name)
        base::close(file_con)
      }, warning=function(w) {
        if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
          invokeRestart("muffleWarning")
      })
      null_replace_columns <- c("e", "v", "vmin", "vmax", "vmean", "f", "fmin", "fmax")
      for (col in null_replace_columns) {
        RSQLite::dbExecute(con, sprintf("UPDATE timeseries SET %s = NULL WHERE %s = ''", col, col))
      }
      
      self$drop_repeated_headers()
      
      RSQLite::dbExecute(con, "ALTER TABLE timeseries ADD d INT DEFAULT 0")
      RSQLite::dbExecute(con, "UPDATE timeseries SET d = d_key")

      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS circuit_details_raw")
      
      RSQLite::dbExecute(con, "CREATE TABLE circuit_details_raw(
                         c_id INT PRIMARY KEY,
                         site_id INT,
                         con_type TEXT,
                         polarity REAL)")
    
      # Suppress warning
      withCallingHandlers({
        file_con <- base::file(circuit_details)
        sqldf::read.csv.sql(sql = circuit_details_build_query, eol = '\n',
                            dbname = self$db_path_name)
        base::close(file_con)
      }, warning=function(w) {
        if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
          invokeRestart("muffleWarning")
      })
      
      RSQLite::dbExecute(con, "ALTER TABLE circuit_details_raw ADD manual_droop_compliance TEXT DEFAULT 'Not set'")
      
      RSQLite::dbExecute(con, "ALTER TABLE circuit_details_raw ADD manual_reconnect_compliance TEXT DEFAULT 'Not set'")
      
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS site_details_raw")
      
      RSQLite::dbExecute(con, "CREATE TABLE site_details_raw(
                         site_id INT, s_postcode INT, s_state TEXT, ac REAL, dc REAL, manufacturer TEXT, model TEXT,
                         pv_installation_year_month TEXT)")
      
      site_details <- read.csv(file = site_details, header = TRUE, stringsAsFactors = FALSE)
      
      # Suppress warning
      withCallingHandlers({
        sqldf::sqldf(site_details_build_query, 
                     dbname = self$db_path_name)
      }, warning=function(w) {
        if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
          invokeRestart("muffleWarning")
      })
      
      
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS alerts")
      
      RSQLite::dbExecute(con, "CREATE TABLE alerts(
                         c_id INT, GridFaultContactorTrip INT, SYNC_a038_DoOpenArguments INT, count_times_open INT, 
                         first_timestamp INT, SYNC_a010_vfCheckFreqWobble INT, SYNC_a005_vfCheckUnderVoltage INT)")
      
      if (!is.null(alerts_file)){
        # Suppress warning
        withCallingHandlers({
          file_con <- base::file(alerts_file)
          sqldf::read.csv.sql(sql = alerts_build_query, eol = '\n',
                              dbname = self$db_path_name)
          base::close(file_con)
        }, warning=function(w) {
          if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
            invokeRestart("muffleWarning")
        })
      }
      
      RSQLite::dbExecute(con, "UPDATE alerts SET GridFaultContactorTrip = NULL WHERE GridFaultContactorTrip = ''")
      RSQLite::dbExecute(con, "UPDATE alerts SET SYNC_a038_DoOpenArguments = NULL 
                         WHERE SYNC_a038_DoOpenArguments = ''")
      RSQLite::dbExecute(con, "UPDATE alerts SET count_times_open = NULL WHERE count_times_open = ''")
      RSQLite::dbExecute(con, "UPDATE alerts SET first_timestamp = NULL WHERE first_timestamp = ''")
      RSQLite::dbExecute(con, "UPDATE alerts SET SYNC_a010_vfCheckFreqWobble = NULL 
                         WHERE SYNC_a010_vfCheckFreqWobble = ''")
      RSQLite::dbExecute(con, "UPDATE alerts SET SYNC_a005_vfCheckUnderVoltage = NULL 
                         WHERE SYNC_a005_vfCheckUnderVoltage = ''")
        
      if (check_dataset_ids_match) {
        self$check_ids_match_between_datasets()
      }
      
      RSQLite::dbDisconnect(con)
    },
    get_time_series_build_query = function(timeseries){
      column_names <- names(read.csv(timeseries, nrows=3, header = TRUE))
      optional_columns <- c("vmin", "vmax", "vmean", "fmin", "fmax")
  
      if (any(optional_columns %in% column_names)){
        replace_columns <- paste(optional_columns[optional_columns %in% column_names], collapse=", ")
        select_columns <- c()
        for (col in optional_columns[optional_columns %in% column_names]) {
          select_columns <- c(select_columns, sprintf("_%s as %s", col, col))
        }
        select_columns <- paste(select_columns, collapse=",\n")
        replace_columns <- paste(", ", replace_columns)
        select_columns <- paste(", ", select_columns)
      } else {
        replace_columns <- ''
        select_columns <- ''
      }

      query <- sprintf("
        REPLACE INTO timeseries(ts, c_id, d_key, e, v, f %s)
        SELECT _ts as ts, 
              cast(_c_id as integer) as c_id, 
              cast(IFNULL(_d, 0) as integer) as d_key, 
              _e as e, 
              _voltage as v,
              _frequency as f
              %s
        from file_con", replace_columns, select_columns)
      
      for (name in column_names){
        if (name %in% names(self$default_timeseries_column_aliases)){
          query <- gsub(self$default_timeseries_column_aliases[[name]], name, query)
        } else {
          error_message <- "The provided time series file should have the columns ts, c_id, d e, v and f, 
          or known aliases of these columns. 
          
          The columns _cols_ where found instead.
          
          Please overide the column alaises in the database interface and try again."
          
          error_message <- gsub('_cols_', paste(column_names, collapse=', '), error_message)
          
          stop(error_message)
        }
      }
      return(query)
    },
    get_circuit_details_build_query = function(circuit_details){
      column_names <- names(read.csv(circuit_details, nrows=3, header = TRUE))
      
      column_aliases <- list(c_id = '_c_id', site_id = '_site_id', 
                             con_type = '_con_type', polarity = '_polarity')
      
      query <- "REPLACE INTO circuit_details_raw  
      SELECT cast(_c_id as integer) as c_id, cast(_site_id as integer) as site_id, _con_type as con_type,
      _polarity as polarity from file_con"
      
      for (name in column_names){
        if (name %in% names(column_aliases)){
          query <- gsub(column_aliases[[name]], name, query)
        } else {
          stop("The provided circuit details file should have the columns c_id, site_id, con_type
               and polarity. Please check this file and try again.")
        }
      }
      return(query)
    },
    get_site_details_build_query = function(site_details){
      column_names <- names(read.csv(site_details, nrows=3, header = TRUE))
      
      column_aliases <- list(site_id='_site_id', s_state='_s_state', ac='_ac',
                             dc='_dc', manufacturer='_manufacturer',
                             model='_model', s_postcode='_s_postcode',
                             pv_installation_year_month='_pv_installation_year_month')
      
      query <- "REPLACE INTO site_details_raw 
      SELECT cast(_site_id as integer) as site_id, cast(_s_postcode as integer) as s_postcode, _s_state as s_state, 
      _ac as ac, _dc as dc, _manufacturer as manufacturer,
      _model as model, _pv_installation_year_month as
      pv_installation_year_month from site_details"
      
      for (name in column_names){
        if (name %in% names(column_aliases)){
          query <- gsub(column_aliases[[name]], name, query)
        } else {
          stop("The provided site details file should have the columns site_id, s_postcode, s_state,
               ac, dc, manufacturer, model and pv_installation_year_month. The ac column should be in
               kW and the the dc in W. Please check this file and try again.")
        }
      }
      return(query)
    },
    get_alerts_build_query = function(alerts_file){
      column_names <- names(read.csv(alerts_file, nrows=3, header = TRUE))
      
      column_aliases <- list(c_id='_c_id', GridFaultContactorTrip='_GridFaultContactorTrip', 
                             SYNC_a038_DoOpenArguments='_SYNC_a038_DoOpenArguments', 
                             count_times_open='_count_times_open', 
                             first_timestamp='_first_timestamp', 
                             SYNC_a010_vfCheckFreqWobble='_SYNC_a010_vfCheckFreqWobble', 
                             SYNC_a005_vfCheckUnderVoltage='_SYNC_a005_vfCheckUnderVoltage')
      
      query <- "REPLACE INTO alerts 
      SELECT cast(_c_id as integer) as c_id, _GridFaultContactorTrip as GridFaultContactorTrip, 
      _SYNC_a038_DoOpenArguments as SYNC_a038_DoOpenArguments, 
      _count_times_open as count_times_open, _first_timestamp as first_timestamp, 
      _SYNC_a010_vfCheckFreqWobble as SYNC_a010_vfCheckFreqWobble,
      _SYNC_a005_vfCheckUnderVoltage as SYNC_a005_vfCheckUnderVoltage
      from file_con"
      
      for (name in column_names){
        if (name %in% names(column_aliases)){
          query <- gsub(column_aliases[[name]], name, query)
        } else {
          stop("The provided alerts file should have the columns c_id, GridFaultContactorTrip, 
          SYNC_a038_DoOpenArguments, count_times_open, first_timestamp, SYNC_a010_vfCheckFreqWobble and 
          SYNC_a005_vfCheckUnderVoltage. Please check this file and try again.")
        }
      }
      return(query)
    },
    drop_repeated_headers = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      start_query <- "DELETE FROM timeseries where ts in ('"
      header_values <- paste(names(self$default_timeseries_column_aliases), collapse="','")
      end_query <- "')"
      query <- paste0(start_query, header_values, end_query)
      RSQLite::dbExecute(con, query)
      RSQLite::dbDisconnect(con)
    },
    check_ids_match_between_datasets = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      c_ids_in_timeseries <- RSQLite::dbGetQuery(con, "SELECT DISTINCT c_id FROM timeseries")
      c_ids_in_circuit_details <- RSQLite::dbGetQuery(con, "SELECT DISTINCT c_id FROM circuit_details_raw")
      site_ids_in_circuit_details <- RSQLite::dbGetQuery(con, "SELECT DISTINCT site_id FROM circuit_details_raw")
      site_ids_in_site_details <- RSQLite::dbGetQuery(con, "SELECT DISTINCT site_id FROM site_details_raw")
      c_ids_intersection <- intersect(c_ids_in_timeseries$c_id, c_ids_in_circuit_details$c_id)
      site_ids_intersection <- intersect(site_ids_in_circuit_details$site_id, site_ids_in_site_details$site_id)
      if (length(c_ids_intersection) < 10) {
        RSQLite::dbExecute(con, "DROP TABLE IF EXISTS timeseries")
        RSQLite::dbExecute(con, "DROP TABLE IF EXISTS circuit_details_raw")
        RSQLite::dbExecute(con, "DROP TABLE IF EXISTS site_details_raw")
        RSQLite::dbDisconnect(con)
        stop("Database build aborted because fewer than 10 ids match between the timeseries and circuit details files.
              Please check files from matching event data sets are being used. To disable this check, use
              check_dataset_ids_match=FALSE.")
      }
      if (length(site_ids_intersection) < 10) {
        RSQLite::dbExecute(con, "DROP TABLE IF EXISTS timeseries")
        RSQLite::dbExecute(con, "DROP TABLE IF EXISTS circuit_details_raw")
        RSQLite::dbExecute(con, "DROP TABLE IF EXISTS site_details_raw")
        RSQLite::dbDisconnect(con)
        stop("Database build aborted because fewer than 10 ids match between the site details and circuit details files.
              Please check files from matching event data sets are being used. To disable this check, use
              check_dataset_ids_match=FALSE.")
      }
      RSQLite::dbDisconnect(con)
    },
    run_data_cleaning_loop = function(max_chunk_size=500){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      
      ids <- RSQLite::dbGetQuery(con, "SELECT DISTINCT c_id FROM timeseries")
      circuit_details <- self$get_circuit_details_raw()
      site_details <- self$get_site_details_raw()
      postcode_data <- self$get_postcode_lon_lat()
      manufacturer_mapping <- self$get_manufacturer_mapping()
      
      site_details <- left_join(site_details, manufacturer_mapping, by = c('manufacturer' = 'sa'))
      site_details <- mutate(site_details, manufacturer = aemo)
      site_details <- site_details[, !(colnames(site_details) %in% c("sa","aemo"))]
      
      site_details <- site_details_data_cleaning_one(site_details)

      # Setup for first iteration.
      length_ids <- length(site_details$site_id)
      iteration_number <- 1
      start_chunk_index <- self$calc_start_chunk_index(iteration_number, max_chunk_size)
      end_chunk_index <- self$calc_end_chunk_index(length_ids, max_chunk_size, start_chunk_index)
      sites_in_chunk <- self$fiter_dataframe_by_start_and_end_index(site_details, start_chunk_index, end_chunk_index)
      circuits <- filter(circuit_details, site_id %in% sites_in_chunk$site_id)
      
      site_details_cleaned <- data.frame()
      circuit_details_cleaned <- data.frame()
      
      while (length(circuits$c_id) > 0){
        time_series <- self$get_time_series_data_by_c_id_full_row(circuits)
        time_series <- remove_outlying_voltages(time_series)
        records_to_update <- filter(time_series, v_changed)
        records_to_update <- within(time_series, rm(v_changed))
        self$update_timeseries_table_in_database(records_to_update)

        time_series <- mutate(time_series, d = as.numeric(d))
        time_series <- mutate(time_series, time = fastPOSIXct(ts, tz="Australia/Brisbane"))
        time_series_5s_and_unknown_durations <- filter(time_series, (!d %in% c(30, 60)))
        time_series_30s_and_60s_data <- filter(time_series, (d %in% c(30, 60)))
        time_series_5s_and_unknown_durations <- self$clean_duration_values(time_series_5s_and_unknown_durations)
        records_to_update <- self$filter_out_unchanged_records(time_series_5s_and_unknown_durations)
        self$update_timeseries_table_in_database(records_to_update)
        time_series <- bind_rows(time_series_30s_and_60s_data, time_series_5s_and_unknown_durations)
        time_series <- mutate(time_series, ts = time)
   
        time_series <- self$add_meta_data_to_time_series(time_series, circuit_details)
        time_series <- self$perform_power_calculations(time_series)
        
        time_series <- filter(time_series, is.finite(power_kW))
        pv_time_series <- filter(time_series, con_type %in% c("pv_site_net", "pv_site", "pv_inverter_net", "pv_inverter"))
        pv_time_series <- pv_time_series[order(pv_time_series$d),]
        pv_time_series <- distinct(pv_time_series, ts, c_id, .keep_all = TRUE)
        
        site_details_cleaned_chunk <- site_details_data_cleaning_two(pv_time_series, sites_in_chunk)
        site_details_cleaned <- bind_rows(site_details_cleaned, site_details_cleaned_chunk)
        
        details_to_add <- select(site_details_cleaned_chunk,  site_id, s_postcode, ac)
        time_series <- inner_join(time_series, details_to_add, by='site_id')
        
        circuit_details_cleaned_chunk <- clean_connection_types(time_series, circuits, postcode_data)
        circuit_details_cleaned <- bind_rows(circuit_details_cleaned, circuit_details_cleaned_chunk)

        
        print(paste0("Done cleaning batch ", iteration_number))

        # Setup for next iteration.
        iteration_number <- iteration_number + 1
        start_chunk_index <- self$calc_start_chunk_index(iteration_number, max_chunk_size)
        end_chunk_index <- self$calc_end_chunk_index(length_ids, max_chunk_size, start_chunk_index)
        sites_in_chunk <- self$fiter_dataframe_by_start_and_end_index(site_details, start_chunk_index, end_chunk_index)
        circuits <- filter(circuit_details, site_id %in% sites_in_chunk$site_id)
        if (start_chunk_index > length_ids){circuits <- ids[0:0,,drop=FALSE]}
  
      }
      
      self$create_site_details_cleaned_table()
      self$insert_site_details_cleaned(site_details_cleaned)
      
      
      self$create_circuit_details_cleaned_table()
      self$insert_circuit_details_cleaned(circuit_details_cleaned)
      
      RSQLite::dbDisconnect(con)
    },
    get_circuit_details_raw = function(){
      circuit_details_raw <- sqldf::sqldf("select * from circuit_details_raw", dbname = self$db_path_name)
      return(circuit_details_raw)
    },
    get_site_details_raw = function(){
      site_details_raw <- sqldf::sqldf("select * from site_details_raw", dbname = self$db_path_name)
      return(site_details_raw)
    },
    get_site_details_cleaned = function(){
      site_details_cleaned <- sqldf::sqldf("select site_id, s_state, s_postcode, ac,
                                                   manufacturer, model, pv_installation_year_month
                                              from site_details_cleaned", 
                                           dbname = self$db_path_name)
      return(site_details_cleaned)
    },
    get_circuit_details_cleaned = function(){
      circuit_details_cleaned <- sqldf::sqldf("select c_id, site_id, con_type, polarity, 
                                                      manual_droop_compliance, 
                                                      manual_reconnect_compliance
                                                 from circuit_details_cleaned", 
                                              dbname = self$db_path_name)
      return(circuit_details_cleaned)
    },
    get_alerts_data = function(){
      alerts_data <- sqldf::sqldf("select * from alerts", 
                                           dbname = self$db_path_name)
      return(alerts_data)
    },
    get_site_details_cleaning_report = function(){
      site_details_cleaned <- sqldf::sqldf("select * from site_details_cleaned", 
                                           dbname = self$db_path_name)
      return(site_details_cleaned)
    },
    get_circuit_details_cleaning_report = function(){
      circuit_details_cleaned <- sqldf::sqldf("select * from circuit_details_cleaned", 
                                              dbname = self$db_path_name)
      return(circuit_details_cleaned)
    },
    get_site_details_processed = function(){
      site_details_processed <- sqldf::sqldf("select * from site_details_processed", 
                                             dbname = self$db_path_name)
      return(site_details_processed)
    },
    get_time_series_data_by_c_id = function(c_ids){
      time_series <- sqldf::sqldf(
        "SELECT ts, c_id, d, e,
        v AS v__numeric, 
        vmin AS vmin__numeric,
        vmax AS vmax__numeric, 
        vmean AS vmean__numeric, 
        f AS f__numeric,
        fmin AS fmin__numeric,
        fmax AS fmax__numeric
        FROM timeseries
        WHERE c_id IN (SELECT c_id FROM c_ids)", 
        dbname = self$db_path_name, method="name__class")
      return(time_series)
    },
    get_time_series_data_by_c_id_full_row = function(c_ids){
      time_series <- sqldf::sqldf(
        "SELECT ts, c_id, d_key, d, e,
        v AS v__numeric, 
        vmin AS vmin__numeric,
        vmax AS vmax__numeric, 
        vmean AS vmean__numeric, 
        f AS f__numeric,
        fmin AS fmin__numeric,
        fmax AS fmax__numeric
        FROM timeseries
        where c_id in 
        (select c_id from c_ids)", 
        dbname = self$db_path_name, method="name__class")
      return(time_series)
    },
    get_time_series_data = function(){
      time_series <- sqldf::sqldf(
        "SELECT ts, c_id, d, e,
        v AS v__numeric, 
        vmin AS vmin__numeric,
        vmax AS vmax__numeric, 
        vmean AS vmean__numeric, 
        f AS f__numeric,
        fmin AS fmin__numeric,
        fmax AS fmax__numeric
        FROM timeseries", 
        dbname = self$db_path_name, method="name__class")
      time_series <- time_series[with(time_series, order(c_id, ts)), ]
      rownames(time_series) <- NULL
      return(time_series)
    },
    get_time_series_data_all = function(){
      time_series <- sqldf::sqldf(
        "SELECT ts, c_id, d_key, d, e,
        v AS v__numeric, 
        vmin AS vmin__numeric,
        vmax AS vmax__numeric, 
        vmean AS vmean__numeric, 
        f AS f__numeric,
        fmin AS fmin__numeric,
        fmax AS fmax__numeric
        FROM timeseries", 
        dbname = self$db_path_nam, method="name__class")
      return(time_series)
    },
    get_filtered_time_series_data = function(state, duration, start_time, end_time){
      circuit_details = self$get_circuit_details_raw()
      site_details = self$get_site_details_raw()
      site_in_state = filter(site_details, s_state==state)
      circuit_in_state = filter(circuit_details, site_id %in% site_in_state$site_id)
      # TODO: check validity of e != ''
      query <- "select ts, c_id, d, e, v, vmin, vmax, vmean, f, fmin, fmax from timeseries 
                        where c_id in (select c_id from circuit_in_state)
                        and d = duration
                        and e != ''
                        and datetime(ts) >= datetime('start_time')
                        and datetime(ts) <= datetime('end_time')"
      query <- gsub('duration', duration, query)
      query <- gsub('start_time', start_time, query)
      query <- gsub('end_time', end_time, query)
      time_series <- sqldf::sqldf(query , dbname = self$db_path_name)
      return(time_series)
    },
    get_filtered_time_series_data_all_durations = function(state, start_time, end_time){
      circuit_details = self$get_circuit_details_raw()
      site_details = self$get_site_details_raw()
      site_in_state = filter(site_details, s_state==state)
      circuit_in_state = filter(circuit_details, site_id %in% site_in_state$site_id)
      query <- "select ts, c_id, d, e, v, f from timeseries 
                        where c_id in (select c_id from circuit_in_state)
                        and e != ''
                        and datetime(ts) >= datetime('start_time')
                        and datetime(ts) <= datetime('end_time')"
      query <- gsub('start_time', start_time, query)
      query <- gsub('end_time', end_time, query)
      time_series <- sqldf::sqldf(query , dbname = self$db_path_name)
      return(time_series)
    },
    get_max_circuit_powers = function(state){
      circuit_details = self$get_circuit_details_raw()
      site_details = self$get_site_details_raw()
      site_in_state = filter(site_details, s_state==state)
      circuit_in_state = filter(circuit_details, site_id %in% site_in_state$site_id)
      query <- "select c.c_id as c_id, max(c.e * c.polarity / (c.d * 1000)) as max_power from
                    ((select c_id, d, e from timeseries 
                            where c_id in (select c_id from circuit_in_state)) a
                        inner join
                     (select c_id, polarity from circuit_details_raw) b
                    on a.c_id == b.c_id) c
                group by c.c_id; 
               "
      max_power <- sqldf::sqldf(query , dbname = self$db_path_name)
      max_power <- mutate(max_power, clean = 'raw')
      if (self$check_if_table_exists('circuit_details_cleaned')){
        query <- "select c.c_id as c_id, max(c.e * c.polarity / (c.d * 1000)) as max_power from
                    ((select c_id, d, e from timeseries 
                            where c_id in (select c_id from circuit_in_state)) a
                        inner join
                     (select c_id, polarity from circuit_details_cleaned) b
                    on a.c_id == b.c_id) c
                group by c.c_id; 
               "
        max_power_clean <- sqldf::sqldf(query , dbname = self$db_path_name)
        max_power_clean <- mutate(max_power_clean, clean = 'clean')
        max_power <- bind_rows(max_power, max_power_clean)
      }
      return(max_power)
    },
    get_postcode_lon_lat = function(){
      postcodes <- sqldf::sqldf("select * from postcode_lon_lat", dbname = self$db_path_name)
      return(postcodes)
    },
    get_manufacturer_mapping = function(){
      postcodes <- sqldf::sqldf("select * from manufacturer_mapping", dbname = self$db_path_name)
      return(postcodes)
    },
    calc_start_chunk_index = function(iteration_number, max_chunk_size){
      start_chunk_index <- (iteration_number - 1) * max_chunk_size + 1
      return(start_chunk_index)
    },
    calc_end_chunk_index = function(number_of_ids, max_chunk_size, start_chunk_index){
      end_chunk_index <- start_chunk_index + max_chunk_size - 1
      if (end_chunk_index > number_of_ids){end_chunk_index <- number_of_ids}
      return(end_chunk_index)
    },
    fiter_dataframe_by_start_and_end_index = function(df, start_index, end_index){
      df <- df[start_index:end_index,,drop=F]
      return(df)
    },
    clean_duration_values = function(time_series){
      time_series <- self$calc_interval_between_measurements(time_series)
      time_series <- self$flag_duration_for_updating_if_value_non_standard_and_calced_interval_is_5s(time_series)
      time_series <- self$replace_duration_value_with_calced_interval(time_series)
      return(time_series)
    },
    calc_interval_between_measurements = function(time_series){
      time_series <- time_series %>% dplyr::group_by(c_id) %>% 
        dplyr::mutate(interval = time - lag(time, order_by = time))
      return(time_series)
    },
    filter_out_unchanged_records = function(time_series){
      time_series <- dplyr::filter(time_series, d_change)
      time_series <- select(time_series, ts, c_id, d_key, d, e, v, vmin, vmax, vmean, f, fmin, fmax)
      return(time_series)
    },
    replace_duration_value_with_calced_interval = function(time_series){
      time_series <- dplyr::mutate(time_series, d=ifelse(d_change, interval, d))
      return(time_series)
    },
    flag_duration_for_updating_if_value_non_standard_and_calced_interval_is_5s = function(time_series){
      time_series <- dplyr::mutate(time_series, d_change=ifelse((!d %in% c(5, 30, 60) & (interval == 5)), TRUE, FALSE))
      return(time_series)
    },
    update_timeseries_table_in_database = function(time_series){
      # Suppress warning
      withCallingHandlers({
        sqldf::sqldf(
          "REPLACE INTO timeseries SELECT ts, c_id, d_key, e, v, vmin, vmax, vmean, f, fmin, fmax, d FROM time_series", 
          dbname = self$db_path_name)
      }, warning=function(w) {
        if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
          invokeRestart("muffleWarning")
      })
    },
    perform_power_calculations = function(time_series){
      time_series <- mutate(time_series, d = as.numeric(d))
      time_series <- mutate(time_series, e = as.numeric(e))
      time_series <- mutate(time_series, e_polarity=e*polarity)
      time_series <- mutate(time_series, power_kW = e_polarity/(d * 1000))
      return(time_series)
    },
    add_meta_data_to_time_series = function(time_series, circuit_details){
      details_to_add <- select(circuit_details, c_id, site_id, polarity, con_type)
      time_series <- inner_join(time_series, details_to_add, by='c_id')
      return(time_series)
    },
    create_site_details_cleaned_table = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS site_details_cleaned")
      RSQLite::dbExecute(con, "CREATE TABLE site_details_cleaned(
                                 site_id INT PRIMARY KEY, s_postcode INT, s_state TEXT, pv_installation_year_month TEXT, 
                                 ac REAL, dc REAL, ac_old REAL, dc_old REAL, ac_dc_ratio REAL, manufacturer TEXT, 
                                 model TEXT, rows_grouped REAL, max_power_kW REAL, change_ac INT, change_dc INT)")
      RSQLite::dbDisconnect(con)
    },
    insert_site_details_cleaned = function(site_details){
      query <- "INSERT INTO site_details_cleaned
                            SELECT site_id, s_postcode, s_state, pv_installation_year_month, ac, dc, ac_old, dc_old, 
                                   ac_dc_ratio, manufacturer, model, rows_grouped, max_power_kW, change_ac, change_dc
                              FROM site_details"
      # Suppress warning
      withCallingHandlers({
        sqldf::sqldf(query, dbname = self$db_path_name)
      }, warning=function(w) {
        if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
          invokeRestart("muffleWarning")
      })
    },
    update_site_details_cleaned = function(site_details){
      query <- "REPLACE INTO site_details_cleaned
                            SELECT site_id, s_postcode, s_state, pv_installation_year_month, ac, dc, ac_old, dc_old, 
                                   ac_dc_ratio, manufacturer, model, rows_grouped, max_power_kW, change_ac, change_dc 
                              FROM site_details"
      withCallingHandlers({
        sqldf::sqldf(query, dbname = self$db_path_name)
      }, warning=function(w) {
        if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
          invokeRestart("muffleWarning")
      })
    },
    create_circuit_details_cleaned_table = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS circuit_details_cleaned")
      RSQLite::dbExecute(con, "CREATE TABLE circuit_details_cleaned(
                         c_id INT PRIMARY KEY, site_id INT, con_type TEXT, polarity INT, sunrise TEXT, sunset TEXT, 
                         min_power REAL, max_power REAL, frac_day REAL, old_con_type TEXT,
                         con_type_changed INT, polarity_changed INT, manual_droop_compliance TEXT, 
                         manual_reconnect_compliance TEXT)")
      RSQLite::dbDisconnect(con)
    },
    insert_circuit_details_cleaned = function(circuit_details){
      query <- "INSERT INTO circuit_details_cleaned
                            SELECT c_id, site_id, con_type, polarity, sunrise, sunset, min_power, max_power, frac_day, 
                                   old_con_type, con_type_changed, polarity_changed, manual_droop_compliance, 
                                   manual_reconnect_compliance
                              FROM circuit_details"
      # Suppress warning
      withCallingHandlers({
        sqldf::sqldf(query, dbname = self$db_path_name)
      }, warning=function(w) {
        if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
          invokeRestart("muffleWarning")
      })
    },
    update_circuit_details_cleaned = function(circuit_details){
      query <- "REPLACE INTO circuit_details_cleaned
                            SELECT c_id, site_id, con_type, polarity, sunrise, sunset, min_power, max_power, frac_day, 
                                   old_con_type, con_type_changed, polarity_changed, manual_droop_compliance, 
                                   manual_reconnect_compliance
                              FROM circuit_details"
      # Suppress warning
      withCallingHandlers({
        sqldf::sqldf(query, dbname = self$db_path_name)
      }, warning=function(w) {
        if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
          invokeRestart("muffleWarning")
      })
    },
    update_circuit_details_raw = function(circuit_details){
      query <- "REPLACE INTO circuit_details_raw
                            SELECT c_id, site_id,  con_type, polarity, manual_droop_compliance, 
                                   manual_reconnect_compliance
                              FROM circuit_details"
      # Suppress warning
      withCallingHandlers({
        sqldf::sqldf(query, dbname = self$db_path_name)
      }, warning=function(w) {
        if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
          invokeRestart("muffleWarning")
      })
    },
    add_postcode_lon_lat_to_database = function(file_path_name){
      if (!file.exists(file_path_name)){
        stop("Specified postcode lon lat file not found. Please check the filepath provided.")
      }
      self$create_postcode_lon_lat_table()
      self$insert_postcode_lon_lat_cleaned(file_path_name)
    },
    create_postcode_lon_lat_table = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS postcode_lon_lat")
      RSQLite::dbExecute(con, "CREATE TABLE postcode_lon_lat(postcode INT PRIMARY KEY, lon REAL, lat REAL)")
      RSQLite::dbDisconnect(con)
    },
    insert_postcode_lon_lat_cleaned = function(file_path_name){
      postcode_lon_lat_df <- read.csv(file = file_path_name, header = TRUE, stringsAsFactors = FALSE)
      query <- "REPLACE INTO postcode_lon_lat SELECT * FROM postcode_lon_lat_df"
      # Suppress warning
      withCallingHandlers({
        invisible(sqldf::sqldf(query, dbname = self$db_path_name)) # stops an empty df being prinited.
      }, warning=function(w) {
        if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
          invokeRestart("muffleWarning")
      })
    },
    add_manufacturer_mapping_table = function(file_path_name){
      if (!file.exists(file_path_name)){
        stop("Specified manufacturer mapping file not found. Please check the filepath provided.")
      }
      self$create_manufacturer_mapping_table()
      self$insert_manufacturer_mapping(file_path_name)
    },
    create_manufacturer_mapping_table = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      RSQLite::dbExecute(con, "DROP TABLE IF EXISTS manufacturer_mapping")
      RSQLite::dbExecute(con, "CREATE TABLE manufacturer_mapping(sa TEXT PRIMARY KEY, aemo TEXT)")
      RSQLite::dbDisconnect(con)
    },
    insert_manufacturer_mapping = function(file_path_name){
      manufacturer_mapping_df <- read.csv(file = file_path_name, header = TRUE, stringsAsFactors = FALSE)
      query <- "REPLACE INTO manufacturer_mapping SELECT * FROM manufacturer_mapping_df"
      # Suppress warning
      withCallingHandlers({
        invisible(sqldf::sqldf(query, dbname = self$db_path_name)) # stops an empty df being prinited.
      }, warning=function(w) {
        if (startsWith(conditionMessage(w), "Don't need to call dbFetch()"))
          invokeRestart("muffleWarning")
      })
    },
    get_min_timestamp = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      min_timestamp <- RSQLite::dbGetQuery(con, "SELECT MIN(ts) as ts FROM timeseries")
      RSQLite::dbDisconnect(con)
      return(fastPOSIXct(min_timestamp$ts[1], tz="Australia/Brisbane"))
    },
    get_max_timestamp = function(){
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      max_timestamp <- RSQLite::dbGetQuery(con, "SELECT MAX(ts) as ts FROM timeseries")
      RSQLite::dbDisconnect(con)
      return(fastPOSIXct(max_timestamp$ts[1], tz="Australia/Brisbane"))
    },
    check_if_table_exists = function(table_name){
      query <- "SELECT count(*) as flag FROM sqlite_master WHERE type='table' AND name='table_name'"
      query <- gsub('table_name', table_name, query)
      con <- RSQLite::dbConnect(RSQLite::SQLite(), self$db_path_name)
      exists_flag <- RSQLite::dbGetQuery(con, query)
      RSQLite::dbDisconnect(con)
      return(exists_flag$flag == 1)
    }
  )
)
