get_free_disk_space_in_working_directory <- function() {
  wd <- getwd()
  disk_name <- substr(wd, 1, 2)
  disk_space <- system("wmic logicaldisk get freespace, caption", intern=TRUE)
  disk_space <- read.table(
      text=disk_space,
      header=TRUE,
      stringsAsFactors=FALSE
  )
  disk_space <- filter(disk_space, Caption==disk_name)
  space_in_bytes <- disk_space$FreeSpace[1]
  space_in_gigabytes <- space_in_bytes / 1073741824
  return(space_in_gigabytes)
}
