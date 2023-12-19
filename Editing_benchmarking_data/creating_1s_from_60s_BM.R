# # creating 60sec data into 1sec data from BM data - constant interpolation
# 
# raw_BM=read.csv('D:/DERDAT/DER_disturbance_analysis/data/ref_raw_data.csv') # reading BM data
# raw_60s_BM<-subset(raw_BM, duration==60) # filtering 60sec data
# raw_60s_BM_order<-raw_60s_BM[order(raw_60s_BM$c_id, raw_60s_BM$utc_tstamp),] # order as per c_id and time
# 
# #interpolating to 1s from 60s - (constant)
# raw_1s_BM<-raw_60s_BM_order[rep(seq_len(nrow(raw_60s_BM_order)),each=60),] # creating constant timesteps
# raw_1s_BM$utc_sec <- rep(0:59,nrow(raw_60s_BM))  # adding increments of 1s in a seperate column
# raw_1s_BM$utc_tstamp <- as.POSIXct(raw_1s_BM$utc_tstamp) # changing time format to POSIXCt
# raw_1s_BM$utc_tstamp_new <- raw_1s_BM$utc_tstamp  + raw_1s_BM$utc_sec # creating consequent 1s time step
# raw_1s_BM$utc_tstamp <- raw_1s_BM$utc_tstamp_new
# 
# raw_1s_BM$energy <- raw_1s_BM$energy / 60  # changing energy to match 1s time interval
# raw_1s_BM$duration <- raw_1s_BM$duration / 60 # changing 1min to 1s duration
# raw_1s_BM<-raw_1s_BM[,-8:-9] # deleting the consequent time step row
# 
# write.csv(raw_1s_BM,"D:/DERDAT/DER_disturbance_analysis/data/op4.csv",row.names=FALSE)



###############################################################################################
# # # # creating 60sec data into 1sec data from BM data - linear interpolation
# # 
raw_BM=read.csv('D:/DERDAT/DER_disturbance_analysis/data/ref_raw_data.csv') # reading BM data
raw_60s_BM<-subset(raw_BM, duration==60) # filtering 60sec data
raw_60s_BM_order<-raw_60s_BM[order(raw_60s_BM$c_id, raw_60s_BM$utc_tstamp),] # order as per c_id and time

 #interpolating to 1s from 60s - (constant)
 raw_1s_BM<-raw_60s_BM_order[rep(seq_len(nrow(raw_60s_BM_order)),each=60),] # creating constant timesteps
 raw_1s_BM$utc_sec <- rep(0:59,nrow(raw_60s_BM))  # adding increments of 1s in a seperate column
raw_1s_BM$utc_tstamp <- as.POSIXct(raw_1s_BM$utc_tstamp) # changing time format to POSIXCt
raw_1s_BM$utc_tstamp_new <- raw_1s_BM$utc_tstamp + raw_1s_BM$utc_sec # creating consequent 1s time step
raw_1s_BM$utc_tstamp <- raw_1s_BM$utc_tstamp_new

# linear interpolation power
raw_1s_BM$power_linear <- raw_1s_BM$power
raw_1s_BM <-  raw_1s_BM %>%  mutate(power_linear_lead =  lead(power_linear, 60))
raw_1s_BM$power_linear_interpolate <- raw_1s_BM$power + ((raw_1s_BM$power_linear_lead - raw_1s_BM$power_linear)/60) * raw_1s_BM$utc_sec
raw_1s_BM$power <- raw_1s_BM$power_linear_interpolate

 # linear interpolation energy
raw_1s_BM$energy <- raw_1s_BM$energy / 60  # changing energy to match 1s time interval
raw_1s_BM$energy_linear <- raw_1s_BM$energy
raw_1s_BM <-  raw_1s_BM %>%  mutate(energy_linear_lead =  lead(energy_linear, 60))
raw_1s_BM$energy_linear_interpolate <- raw_1s_BM$energy + ((raw_1s_BM$energy_linear_lead - raw_1s_BM$energy_linear)/60) * raw_1s_BM$utc_sec
raw_1s_BM$energy <- raw_1s_BM$energy_linear_interpolate

raw_1s_BM$duration <- raw_1s_BM$duration / 60 # changing 1min to 1s duration
raw_1s_BM<-raw_1s_BM[,-8:-15] # deleting the consequent time step row
#
 write.csv(raw_1s_BM,"D:/DERDAT/DER_disturbance_analysis/data/op6.csv",row.names=FALSE)
#
# ################################################################################################


# checking number of rows and c_ids
# 
# BM_60sto1s=read.csv('D:/DERDAT/DER_disturbance_analysis/data/interpolated_data60sto1s_v4.csv') # reading BM data
# BM_60sand5s=read.csv('D:/DERDAT/DER_disturbance_analysis/data/ref_raw_data.csv') # reading BM data
# BM_60s<-subset(BM_60sand5s, duration==60) # filtering 60sec data
# 
# num_cid_60sto1s<-nrow(BM_60sto1s)
# num_cid_60s<-nrow(BM_60s)
# 
# print(c(num_cid_60sto1s,num_cid_60s))
# 
# cids_60sto1s<-unique(BM_60sto1s$c_id)
# cids_60s<-unique(BM_60s$c_id)
# 
# print(length(cids_60sto1s))
# print(length(cids_60s))
# cids_60sto1s %in% cids_60s # both dbs consists of the same unique c_ids 
#######################################################################################


  
# # raw_1s_BM$utc_tstamp<-raw_1s_BM$utc_tstamp + sequence(1)
# # utc_tstamp <- sequence(raw_1s_BM$utc_tstamp)
# # 

# group_c_id_1min <- raw_1s_BM %>%
#   group_by(c_id,utc_tstamp)  %>%
#   expand(seq.POSIXt(from=first(utc_tstamp),to=last(utc_tstamp),by=1,units="seconds")) %>%
#   ungroup



#   

# 
# Time.index <- df %>% 
#   group_by(ID) %>% 
#   expand(seq.POSIXt(from =first(df$Time), to = last(df$Time),by = 1, units = "seconds"))%>% 
#   ungroup()
# mutate(Date = as.Date(raw_1s_BM$utc_stamps[1]) + months(seq(0, length.out = n(), by = 1)))
  # summarise(avg_precip = mean(total_precip_col))
  # summarise(utc_tstamp)


# NA_empty_df<-data.frame(matrix(NA, ncol = ncol(raw_BM), nrow = 59))
# raw_1s_BM<- do.call(rbind, apply(raw_60s_BM_order, 1, function(x) {rbind(x, NA_empty_df)}))



# DD1 <- data.frame(a = 1:2, b = letters[1:2]) 
# DD1[rep(seq_len(nrow(DD1)), each = 2), ]




# =read.csv('D:/DERDAT/DER_disturbance_analysis/data/ref_raw_data_60s.csv')
# raw_60s_BM_order<-setorder(raw_60s_BM, c_id, utc_tstamp)

# NA_empty_df<-data.frame(matrix(NA, ncol = ncol(raw_60s_BM), nrow = 4))
# raw_1s_BM<- do.call(rbind, apply(raw_60s_BM, 1, function(x) {rbind(x, NA_empty_df)}))

# print(NA_empty_df)
# 
# 
# my.df <- data.frame(num = 1:5, let = letters[1:5])
# # na.df <- data.frame(num = NA, let = NA)
# m <- matrix(NA, ncol = 2, nrow = 4)
# na.df<-data.frame(m)
# my.df <- do.call(rbind, apply(my.df, 1, function(x) {rbind(x, na.df)}))
# 
# print(my.df)
# 


