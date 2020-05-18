library(R6)
library(sqldf)


DataProcessor <- R6Class("DataProcessor",
  public = list(
    db_path_name = NULL,
    connect_to_existing_database = function(db_path_name){
      "This is the database that will subsequently be used for accessing data.

      \\examples{

        processor <- DataProcessor$new()
  
        processor$connect_to_existing_database('Event_20180825.db')
  
        print(processor$db_path_name)

        \\dontshow{assert('Setting database not working', 
                         processor$db_path_name == 'Event_20180825.db')}

      }
      
      "
      self$db_path_name = db_path_name
    },
    connect_to_new_database = function(db_path_name){
      "Create a new database."
      sqldf(db_path_name)
      # Store its name so it can be accessed later.
      self$db_path_name = db_path_name
    },
    build_database = function(time_series, circuit_details, site_details) {
      "This method takes raw solar analytics data and inserts it into
       an existing sql database. Some minamal data processing/cleaning is
       performed so this data can be subsequently analyised. Further data clean
       can be performed by using the clean_data method."
      x=1
    },
    clean_data = function(){
      # Creates new tables site_details and circuit_details files in the 
      # database with 'clean' contents.
      x=1
    }
  )
)