# Validate benchmarking dataset results
# Step 1: Ensure you have read the README in this directory and followed the instructions on creating the reference
#         and test data
# Step 2: Run this script from DER disturbance analysis directory
#         OR change tool_directory to the location of the DER disturbance analysis repo
# Step 3: Review the results printed to the console. Investigate any differences identified to ensure that only
#         expected changes are present.

logging::basicConfig()

base_directory_name <- basename(getwd())
if (base_directory_name == "DER_disturbance_analysis") {
    tool_directory <- getwd()
} else {
    print("Script is not being run in DER_disturbance_analysis folder, make sure that tool directory ahs been set")
    tool_directory <- "~/UNSW/MATCH/DER_disturbance_analysis"
}
source(sprintf("%s/BDInterface/interface.R", tool_directory))

data_dirs <- list.dirs('validation/data', recursive=FALSE)

ref_db_name <- "ref.db"
test_db_name <- "test.db"
ref_circuit_summary_fname <- "ref_circ_sum.csv"
ref_underlying_data_fname <- "ref_underlying.csv"
test_circuit_summary_fname <- "test_circ_sum.csv"
test_underlying_data_fname <- "test_underlying.csv"

required_files <- c(
    ref_circuit_summary_fname, ref_underlying_data_fname,
    test_circuit_summary_fname, test_underlying_data_fname
)

table_name_query <- "SELECT DISTINCT name FROM sqlite_master WHERE type = 'table'"
table_columns_query <- "PRAGMA table_info('%s')"
table_length_query <- "SELECT count(*) AS length FROM %s"
table_all_data_query <- "SELECT * FROM %s"


difference_between_lists <- function(reference_list, test_list) {
    results <- list()
    results$reference_only <- reference_list[!(reference_list %in% test_list)]
    results$test_only <- test_list[!(test_list %in% reference_list)]
    results$common <- reference_list[(reference_list %in% test_list)]
    return(results)
}


compare_dbs <- function(ref_db_con, test_db_con, compare_values=FALSE, event_name=NA){
    # compare tables
    ref_tables <- RSQLite::dbGetQuery(ref_db_con, table_name_query)$name
    test_tables <- RSQLite::dbGetQuery(test_db_con, table_name_query)$name
    table_diffs <- difference_between_lists(ref_tables, test_tables)

    if (length(table_diffs$reference_only) > 0) {
        logging::loginfo(sprintf("%s - The following tables exist only in the REFERENCE database:\n%s",
                                 event_name, table_diffs$reference_only))
    }
    if (length(table_diffs$test_only) > 0) {
        logging::loginfo(sprintf("%s - The following tables exist only in the TEST database:\n%s",
                                 event_name, table_diffs$test_only))
    }

    # compare columns in tables
    for (table in table_diffs$common) {
        ref_columns <- RSQLite::dbGetQuery(ref_db_con, sprintf(table_columns_query, table))$names
        test_columns <- RSQLite::dbGetQuery(test_db_con, sprintf(table_columns_query, table))$names
        column_diffs <- difference_between_lists(ref_columns, test_columns)

        if (length(column_diffs$reference_only) > 0) {
            logging::loginfo(sprintf("%s - Table %s: the following columns exist only in the REFERENCE data:\n%s",
                                     event_name, table, column_diffs$reference_only))
        }
        if (length(column_diffs$test_only) > 0) {
            logging::loginfo(sprintf("%s - Table %s: the following columns exist only in the TEST data:\n%s",
                                     event_name,  table, column_diffs$test_only))
        }

        # compare volume of data in columns
        ref_table_length <- RSQLite::dbGetQuery(ref_db_con, sprintf(table_length_query, table))
        test_table_length <- RSQLite::dbGetQuery(test_db_con, sprintf(table_length_query, table))

        if (ref_table_length$length[[1]] != test_table_length$length[[1]]) {
            logging::loginfo(sprintf(
                "%s - Table %s has differing lengths in reference and test data\nref: %s; test: %s",
                event_name, table, ref_table_length$length[[1]], test_table_length$length[[1]]
                ))
        }
 
        # compare values in table
        if (compare_values) {
            ref_data <- RSQLite::dbGetQuery(ref_db_con, sprintf(table_all_data_query, table))
            test_data <- RSQLite::dbGetQuery(test_db_con, sprintf(table_all_data_query, table))
            diffs <- all.equal(ref_data[column_diffs$common], ref_data[column_diffs$common])
            if (!isTRUE(diffs)){
                logging::loginfo(
                    sprintf(
                        "%s - Differences found in values:\n%s",
                        event_name, toString(diffs)
                    )
                )
            }
        }
    }
}


compare_dfs <- function(reference, test, event_name=NA){
    # 1. check for new columns
    ref_columns <- names(reference)
    test_columns <- names(test)
    column_diff <- difference_between_lists(ref_columns, test_columns)

    if (length(column_diff$reference_only) > 0) {
        logging::loginfo(
            sprintf(
                "%s - Reference dataframe contains columns not found in test dataframe:\n%s",
                event_name, toString(column_diff$reference_only)
            )
        )
    }
    if (length(column_diff$test_only) > 0) {
        logging::loginfo(
            sprintf(
                "%s - Test dataframe contains new columns:\n%s",
                event_name, toString(column_diff$test_only)
            )
        )
    }

    # 2. check for different values in existing columns
    diffs <- all.equal(reference[column_diff$common], test[column_diff$common])
    if (!isTRUE(diffs)){
        logging::loginfo(
            sprintf(
                "%s - Differences found in column values:\n%s",
                event_name, toString(diffs)
            )
        )
    }
    return(diffs)
}


if (length(data_dirs) > 0){
    for (dir in data_dirs){
        all_files_in_dir <- list.files(dir)
        required_files_in_dir <- required_files %in% all_files_in_dir
        if (all(required_files_in_dir)){
            # check databases
            ref_db_con <- RSQLite::dbConnect(RSQLite::SQLite(), sprintf("%s/%s", dir, ref_db_name))
            test_db_con <- RSQLite::dbConnect(RSQLite::SQLite(), sprintf("%s/%s", dir, test_db_name))
            compare_dbs(ref_db_con, test_db_con, TRUE, dir)

            # check csvs
            ref_circuit_summary <- read.csv(sprintf("%s/%s", dir, ref_circuit_summary_fname))
            ref_underlying_data <- read.csv(sprintf("%s/%s", dir, ref_underlying_data_fname))
            test_circuit_summary <- read.csv(sprintf("%s/%s", dir, test_circuit_summary_fname))
            test_underlying_data <- read.csv(sprintf("%s/%s", dir, test_underlying_data_fname))

            # check circuit summary
            compare_dfs(ref_circuit_summary, test_circuit_summary, event_name=dir)
            # check underlying data
            compare_dfs(ref_underlying_data, test_underlying_data, event_name=dir)

            RSQLite::dbDisconnect(ref_db_con)
            RSQLite::dbDisconnect(test_db_con)
        }
    }
}
