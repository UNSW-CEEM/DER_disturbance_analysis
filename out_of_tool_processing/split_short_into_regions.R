###INPUT CSV FILES
#Rename your csv file into the format: "short_YYYY-MM-DD.csv"
temp.csv <- "short_YYYY-MM-DD.csv"

## Script to split
df.c_id <- read.csv("circuit_details.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  select(site_id, c_id)

df.site_id <- read.csv("site_details.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  select(site_id, s_state)
df.site_id <- distinct(df.site_id)

df.circuit_state <- left_join(df.c_id, df.site_id, by = "site_id") %>%
  select(c_id, s_state) %>%
  distinct()

row_count_file <- NULL
row_count_region_file <- NULL

temp.df <- read.csv(temp.csv, header = TRUE, stringsAsFactors = FALSE)

df.ts_state <- left_join(temp.df, df.circuit_state, by = "c_id")

all_regions <- unique(df.ts_state$s_state)

temp.row_count <- as.data.frame(nrow(temp.df)) %>%
  mutate(new_df_count = nrow(df.ts_state)) %>%
  mutate(File = temp.csv)

names(temp.row_count) <- c("OriginalRowCount", "NewRowCount", "File")

row_count_file <- rbind(row_count_file, temp.row_count)

for (region in all_regions) {
  if (is.na(region) == TRUE) {
    temp.NA.df <- filter(df.ts_state, is.na(s_state))

    na.c_id <- as.data.frame(unique(temp.NA.df$c_id))
    names(na.c_id) <- "UnmatchedCiDs"

    write.csv(na.c_id, paste0(substr(temp.csv, 7, 16), "_", region, "short.csv"))

    temp.row_count_region <- as.data.frame(nrow(temp.NA.df)) %>%
      mutate(File = temp.csv) %>%
      mutate(Region = region)

    names(temp.row_count_region) <- c("RowCount", "File", "Region")
  } else {
    temp.region.df <- filter(df.ts_state, s_state == region) %>%
      select(c_id, utc_tstamp, energy, power, voltage, frequency, duration)

    write.csv(temp.region.df, paste0(substr(temp.csv, 7, 16), "_", region, "short.csv"))

    temp.row_count_region <- as.data.frame(nrow(temp.region.df)) %>%
      mutate(File = temp.csv) %>%
      mutate(Region = region)

    names(temp.row_count_region) <- c("RowCount", "File", "Region")
  }
  row_count_region_file <- rbind(row_count_region_file, temp.row_count_region)
}


write.csv(row_count_file, "Original_total_rows.csv")

write.csv(row_count_region_file, "Indiv_total_rows.csv")
