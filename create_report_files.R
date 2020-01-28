
create_files <- function(upscaled_timeseries, timeseries_by_site, 
                         pre_event_interval, event_time, data_path){
  upscaled_power_by_response <- categorise_upscaled_power(upscaled_timeseries, 
                                                          timeseries_by_site)
  create_plots(upscaled_power_by_response, timeseries_by_site, pre_event_interval, 
               event_time, data_path)
}

categorise_upscaled_power <- function(upscaled_timeseries, timeseries_by_site){
  upscaled_timeseries <- check_for_duplicates(upscaled_timeseries, "Upscaled")
  
  upscaled_timeseries <- dplyr::mutate(upscaled_timeseries, 
                                       Time=ymd_hms(Time,tz="Australia/Brisbane"))
  
  timeseries_by_site <- check_for_duplicates(timeseries_by_site, "Underlying")
  
  timeseries_by_site <- re_name_response_categories(timeseries_by_site)
  
  response_percentage_by_standard <- 
    calc_response_percentage_by_standard(timeseries_by_site)
  
  upscaled_power_by_response <- disaggregate_upscaled_power_by_response(
    upscaled_timeseries, 
    response_percentage_by_standard)
  
  return(upscaled_power_by_response)
}
  

check_for_duplicates <- function(timeseries, data_name){
  temp.distinct <- dplyr::distinct(timeseries)
  print(paste0(data_name ," data has ",nrow(timeseries)," rows, and ",
               nrow(temp.distinct)," rows after duplicates removed."))
  print("please check and investigate if these numbers don't match. 
        Analysis continued on filtered data set.")
  print("_____________")
  return(temp.distinct)
}

re_name_response_categories <- function(timeseries_by_site){
  timeseries_by_site <- timeseries_by_site %>% 
    dplyr::filter(!response_category %in% c("5 Off at t0", "NA", "Undefined", 
                                            "6 Not enough data")) %>% 
    dplyr::mutate(response_category=gsub("3 Drop to Zero", "4 Disconnect",
                                         response_category),
                  response_category=paste(substr(response_category,3,
                                                 length(response_category))),
                  ts=ymd_hms(ts,tz="Australia/Brisbane"),
                  response_category=gsub(" ","-",response_category))
  print("Merged drop to zero with disconnect category, 
        response categories now read: ")
  print(unique(timeseries_by_site$response_category))
  print("_____________")
  return(timeseries_by_site)
}

order_response_categories <- function(data){
  data$response_category <- ordered(
    data$response_category, levels=c("Disconnect", "Curtail", "Ride-Through"))
  return(timeseries_by_site)
}

calc_response_percentage_by_standard <- function(timeseries_by_site){
  timeseries_by_site <- timeseries_by_site %>% 
    select(ts, power_kW, site_performance_factor, Standard_Version, 
           response_category) %>% 
    mutate(key=paste0(Standard_Version,"_",response_category))
  
  aggregate_by_response_category <- aggregate(
    site_performance_factor ~ ts + Standard_Version + response_category, 
    timeseries_by_site, sum) %>% 
    mutate(key=paste0(ts,"_",Standard_Version))
  
  aggregate_by_response_standard <- aggregate(
    site_performance_factor ~ ts + Standard_Version, timeseries_by_site, sum) %>% 
    mutate(key=paste0(ts,"_",Standard_Version))
  
  response_percentage_by_standard <- left_join(
    aggregate_by_response_category,aggregate_by_response_standard,by="key") %>% 
    select(ts=ts.x,key,response_category,Standard_Version=Standard_Version.y,
           res.power=site_performance_factor.x,total.power=site_performance_factor.y) %>% 
    mutate(perc_response=res.power/total.power)
  return(response_percentage_by_standard)
}

disaggregate_upscaled_power_by_response <- function(
  upscaled_timeseries, response_percentage_by_standard){
  
  upscaled_timeseries <- upscaled_timeseries %>% select(Time, Power_kW, 
                                                        Standard_Version)
  
  upscaled_timeseries <- aggregate(Power_kW ~ Time + Standard_Version, 
                                   upscaled_timeseries, sum) %>% 
    mutate(key=paste0(Time,"_",Standard_Version))
  
  upscaled_timeseries <- left_join(
    response_percentage_by_standard, upscaled_timeseries, by="key") %>% 
    filter(!is.na(Power_kW)) %>% 
    select(ts,response_category,Standard_Version=Standard_Version.y,
           kW_sample=res.power,perc_response,Power_kW) %>% 
    mutate(kW_upscaled=perc_response*Power_kW,
           MW_upscaled=kW_upscaled*0.001)
  
  upscaled_power_by_response <- aggregate(
    MW_upscaled ~ ts + response_category + Standard_Version, 
    upscaled_timeseries, sum)
  
  return(upscaled_power_by_response)
}

create_plots <- function(upscaled_timeseries, timeseries_by_site, 
                         pre_event_interval, event_time, data_path){
  
  t0 <- as.POSIXct(pre_event_interval)
  tx <- as.POSIXct(event_time)
  browser()
  ## plot 2 ## raw data PV output short (for the time near the event) ####
  temp.plot <- aggregate(power_kW ~ ts + response_category, timeseries_by_site, 
                         sum) %>% 
    filter(ts>(t0-minutes(5)) & ts <(t0+minutes(15)))
  
  colnames(temp.plot)[2] <- "Legend"
  
  
  p2 <- ggplot(temp.plot, aes(ts,power_kW))+
    geom_area(position="stack",aes(fill=Legend))+
    labs(title="[Raw data] PV output by response category")+
    theme(legend.position="bottom")+
    xlab("Time")+
    ylab("Power (kW)")+
    geom_vline(aes(xintercept=t0),size=0.9,colour="black",linetype="dashed")+
    sapply(tx,function(tx)geom_vline(aes(xintercept=tx),size=0.9,colour="red",linetype="dashed"))+
    scale_fill_manual(values=AEMOCpp)
  
  # plot(p2)
  
  ## remove temps
  rm(list=ls(pattern="temp"))
  
  ## plot 3 ## PV output short (for the time near the event) by respopnse, standard, and size #### 
  temp.plot <- aggregate(power_kW ~ ts + response_category + Grouping + Standard_Version, timeseries_by_site, sum) %>% 
    mutate(Category=paste0(Standard_Version," ",Grouping))%>% 
    filter(ts>(t0-minutes(5)) & ts <(t0+minutes(15))) %>% 
    mutate(CatOrder =ifelse(Category =="AS4777.3:2005 <30 kW", 1, ifelse(Category=="Transition <30 kW", 2, ifelse(Category=="AS4777.2:2015 <30 kW", 3, ifelse(Category=="AS4777.3:2005 30-100kW", 4, ifelse(Category=="Transition 30-100kW", 5, ifelse(Category=="AS4777.2:2015 30-100kW",6,7))))))) %>% 
    mutate(ResponseOrder = ifelse(response_category =="Curtail", 2, ifelse(response_category=="Ride-Through", 1, ifelse(response_category=="Disconnect", 3, 4))))
  
  temp.plot$response_category <- factor(temp.plot$response_category, levels = unique(temp.plot$response_category[order(-temp.plot$ResponseOrder)]))
  temp.plot$Category <- factor(temp.plot$Category, levels = unique(temp.plot$Category[order(temp.plot$CatOrder)]))
  
  
  colnames(temp.plot)[2] <- "Legend"
  
  
  p3 <- ggplot(temp.plot, aes(ts, power_kW))+
    geom_area(position="stack",aes(fill=Legend))+
    facet_wrap(~Category)+
    geom_vline(aes(xintercept=t0),size=0.9,linetype="dashed", colour="black")+
    sapply(tx,function(tx)geom_vline(aes(xintercept=tx),size=0.9,colour="red",linetype="dashed"))+
    theme(legend.position="bottom")+
    xlab("Time")+
    ylab("Power (kW)")+
    scale_fill_manual(values=AEMOCpp)
  
  # plot(p3)
  
  ## remove temps
  rm(list=ls(pattern="temp"))
  
  
  ## plot 4 ## upscaled PV output short (for the time near the event) ####
  temp.plot <- aggregate(MW_upscaled ~ ts + response_category, 
                         upscaled_timeseries, sum)%>% 
    filter(ts>(t0-minutes(5)) & ts <(t0+minutes(15)))
  
  colnames(temp.plot)[2] <- "Legend"
  
  
  p4 <- ggplot(temp.plot, aes(ts, MW_upscaled))+
    geom_area(position="stack",aes(fill=Legend))+
    labs(title="[Upscaled data] PV output by response category")+
    theme(legend.position="bottom")+
    xlab("Time")+
    ylab("Power (MW)")+
    geom_vline(aes(xintercept=t0),size=0.9,colour="black",linetype="dashed")+
    sapply(tx,function(tx)geom_vline(aes(xintercept=tx),size=0.9,colour="red",linetype="dashed"))+
    scale_fill_manual(values=AEMOCpp)
  
  
  # plot(p4)
  
  ## remove temps
  rm(list=ls(pattern="temp"))
  
  ## plot 5 ## upscaled next to raw data. plots 4 and 2 together ####
  
  temp.p <- p4 +
    theme(legend.position = "none")
  temp.p2 <- p2 +
    theme(legend.position = "none")
  
  p5 <- grid.arrange(temp.p2, temp.p, nrow=2)
  
  ## remove temps
  rm(list=ls(pattern="temp"))
  
  ## plot 6 ## response by zone bar graph
  ## collate sample count for standard version, zone, and response of each circuit ID
  temp.sample <- unique(select(timeseries_by_site, c_id,Standard_Version,zone,
                               response_category)) %>% 
    mutate(response_category=gsub("-","_",response_category),
           count=1)
  
  temp.group <- temp.sample %>% 
    group_by(Standard_Version,zone,response_category) %>% 
    summarise(d=sum(count)) %>% 
    ungroup() %>% 
    filter(response_category=="Disconnect") %>% 
    mutate(key=paste0(Standard_Version,"_",zone))
  
  
  ###
  #### create n=0 frame so that the plots will show where n=0
  temp.zones <- unique(temp.sample$zone)
  
  temp.n0 <- sapply(temp.zones,function(x){
    
    temp.bind <-  data.frame(c_id=c(1,2,3),
                             Standard_Version=as.factor(c("AS4777.3:2005", "Transition", "AS4777.2:2015")),
                             zone=x,
                             response_category=as.character("null"),
                             count=0)
    
    bind_rows(temp.bind)},simplify=FALSE)
  
  temp.n0 <- bind_rows(temp.n0)
  
  temp.test <- bind_rows(temp.n0,temp.sample)
  
  ##
  
  temp.group2 <- temp.test %>% 
    group_by(Standard_Version,zone) %>% 
    summarise(n=sum(count)) %>% 
    ungroup() %>% 
    mutate(key=paste0(Standard_Version,"_",zone))
  
  temp.join <- left_join(temp.group2,select(temp.group,key,response_category,d),by="key") %>% 
    mutate(perc_disc=(d/n)*100) %>% 
    select(-response_category,-d,-key)
  
  temp.join[is.na(temp.join)] <- 0
  
  temp.plot <- temp.join
  
  temp.plot$Standard_Version <- ordered(temp.plot$Standard_Version, levels=c("AS4777.3:2005", "Transition", "AS4777.2:2015"))
  temp.plot$lab <- paste0("n=",temp.plot$n)
  
  p6 <- ggplot(temp.plot, aes(zone,perc_disc,colour=Standard_Version))+
    geom_bar(stat="identity",position="dodge",aes(fill=Standard_Version))+
    scale_fill_manual(values=AEMOCpp)+
    # scale_fill_manual(values=c("salmon1","slateblue2","seagreen3"))+
    labs(title="Percentage of PV that disconnected as a percentage of zone/standard groups",
         subtitle="'n' indicates the sample size for each zone/standard group")+
    ylab("Percentage of sites in sample that disconnected (%)")+
    xlab(NULL)+
    theme(legend.position="bottom")+
    geom_text(aes(label=lab,colour=Standard_Version),position=position_dodge(width=1),vjust=-.5,size=3)+
    scale_colour_manual(values=c("black","black","black"))+
    ylim(limits = c(0, 100))
  
  
  temp.plot7 <- temp.sample %>% 
    group_by(zone, response_category) %>% 
    summarise(d=sum(count)) %>% 
    spread(response_category, d)
  temp.plot7[is.na(temp.plot7)] <- 0
  temp.plot7 <- temp.plot7 %>% 
    mutate(Total = sum(Curtail+Disconnect+Ride_Through)) %>% 
    mutate(perc_disc= 100*Disconnect/Total) %>% 
    mutate(lab = paste0("n=",Total))
  
  
  p7 <- ggplot(temp.plot7, aes(zone,perc_disc))+
    geom_bar(stat="identity",position="dodge", aes(fill="#360F3C"), show.legend=FALSE)+
    scale_fill_manual(values=AEMOCpp)+
    labs(title="Percentage of PV that disconnected by zone",
         subtitle="'n' indicates the sample size for each zone")+
    ylab("Percentage of sites in zone that disconnected (%)")+
    xlab(NULL)+
    geom_text(aes(label=lab),position=position_dodge(width=1),vjust=-.5,size=3)
  
  # plot(p6)
  
  ## remove temps
  rm(list=ls(pattern="temp"))
  
  
  ## plot 8 ## cumulative disconnects by standard ####
  
  #Find all unique circuit IDs that have a postcode
  temp.plot8 <- timeseries_by_site[!is.na(timeseries_by_site$s_postcode),]
  temp.plot8 <- distinct(temp.plot8, c_id, .keep_all=TRUE)
  
  #Create new columns to indicate number of sites and number of disconnections by standard version and postcode
  temp.plot8 <- mutate(temp.plot8, num_disconnects=ifelse(response_category=="Disconnect",1,0))
  temp.plot8 <- mutate(temp.plot8, system_count=1)
  temp.plot8 <- temp.plot8[order(temp.plot8$distance),]
  temp.plot8 <- group_by(temp.plot8, Standard_Version, s_postcode)
  temp.plot8 <- summarise(temp.plot8, distance=first(distance), num_disconnects=sum(num_disconnects),
                          system_count=sum(system_count))
  #Create new columns to sum up cumulative disconnections and systems by distance from event and convert to percentage
  temp.plot8 <- temp.plot8[order(temp.plot8$distance),]
  temp.plot8 <- mutate(temp.plot8,cum_num_disconnects=cumsum(num_disconnects))
  temp.plot8 <- mutate(temp.plot8, cum_system_count=cumsum(system_count))
  temp.plot8 <- mutate(temp.plot8, percentage=cum_num_disconnects/cum_system_count)
  
  # plot p8  
  p8 <- ggplot(temp.plot8, aes(x=distance,y=percentage,colour=Standard_Version))+
    geom_point()+
    xlab("Distance from event (km)")+
    ylab("Cumlative  disconnects")+
    labs(title="Percentage of PV that disconnected as a percentage of zone/standard groups")+
    geom_vline(aes(xintercept=zone1),size=0.9,colour="black",linetype="dashed")+
    geom_vline(aes(xintercept=zone2),size=0.9,colour="black",linetype="dashed")+
    geom_vline(aes(xintercept=zone3),size=0.9,colour="black",linetype="dashed")+
    scale_colour_manual(values=AEMOCpp)
  #ensure origin is visible on graph
  p8 <- p8 + expand_limits(x = 0, y = 0)
  
  # plot(p8)
  
  ## remove temps
  rm(list=ls(pattern="temp"))
  
  
  #Plot 9 site performance factor ####
  
  #trim dataset to time near event and create a key for standard version/ response category
  temp.plot9 <- timeseries_by_site %>%
    filter(ts>(t0-minutes(5)) & ts <(t0+minutes(15))) %>%
    mutate(key=paste0(Standard_Version,"_",response_category))
  
  #aggregate circuit output to the site-id level and calculate performance factor for each site/time
  temp.plot9 <- temp.plot9 %>%
    group_by(ts, site_id, key ,sum_ac) %>%
    summarise(site_performance_factor=sum(power_kW)) %>%
    mutate(site_performance_factor=(site_performance_factor/sum_ac))
  
  #find performance factor at the pre-event interval to use for normalising (next step)
  temp.plot9a <- temp.plot9 %>%
    filter(ts == t0) %>%
    mutate(event_site_performance_factor = site_performance_factor)
  
  #normalise all site factors to the pre-event interval 
  temp.plot9 <- temp.plot9 %>%
    left_join(select(temp.plot9a, site_id, key, ts, event_site_performance_factor), by=c("site_id","key")) %>%
    select(ts=ts.x, site_id, key, site_performance_factor, event_site_performance_factor) %>%
    mutate(Event_Normalised_Power_kW=ifelse(event_site_performance_factor>0.00001,
                                            site_performance_factor/event_site_performance_factor, NA))
  
  #find averages for Standard version/response category for graphing
  temp.plot9 <- temp.plot9 %>%
    group_by(ts, key) %>%
    summarise(Average_Event_Normalised_Power_kW=mean(Event_Normalised_Power_kW))
  
  #colours for plot 9
  getPalette = grDevices::colorRampPalette(c("#360F3C", "#C41230", "#F37421", "#FFC222"))
  colourCount = length(unique(temp.plot9$key))
  
  #create p9 plot
  p9 <-  ggplot(temp.plot9, aes(ts, Average_Event_Normalised_Power_kW, colour = key))+
    geom_line()+
    scale_colour_manual(values = getPalette(colourCount))+
    xlab("Time")+
    ylab("Average site performance factor \n normalised to value of pre-event interval")
  
  # plot(p9)
  
  ## remove temps
  rm(list=ls(pattern="temp"))
  
  ##### 6. save outputs ####
  #setwd(paste0("",directory,"/PP_output_",event_date,""))
  
  ggsave(p2,file=paste0("plot_2_",savetime,".png"),height =7, width =10, path=data_path)
  ggsave(p3,file=paste0("plot_3_",savetime,".png"),height =7, width =10, path=data_path)
  ggsave(p4,file=paste0("plot_4_",savetime,".png"),height =7, width =10, path=data_path)
  ggsave(p5,file=paste0("plot_5_",savetime,".png"),height =7, width =10, path=data_path)
  ggsave(p6,file=paste0("plot_6_",savetime,".png"),height =7, width =10, path=data_path)
  ggsave(p7,file=paste0("plot_7_",savetime,".png"),height =7, width =10, path=data_path)
  ggsave(p8,file=paste0("plot_8_",savetime,".png"),height =7, width =10, path=data_path)
  ggsave(p9,file=paste0("plot_9_",savetime,".png"),height =7, width =10, path=data_path)
}