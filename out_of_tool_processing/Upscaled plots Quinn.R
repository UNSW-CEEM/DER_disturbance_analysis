UnderlyingData <- read.csv("C://Users//qpatterson//Documents//My Documents//DER//Events//2018-08-25//Cleaned//FINAL//NSW 60s_underlying.csv",stringsAsFactors=FALSE)
APVICapacity <- read.csv("C://Users//qpatterson//Documents//My Documents//DER//DER_disturbance_analysis-nick_dev//DER_disturbance_analysis-nick_dev//cumulative_capacity_and_number.csv")
DateOfEvent <- "2018-08-25"
Region <- "NSW"

library(dplyr)
library(ggplot2)
library(extrafont)
# The line below may need to be run first time to load Segoe UI Semilight (preferred font)
# https://www.rdocumentation.org/packages/extrafont/versions/0.17
#font_import()
loadfonts(device="win")

### Information required: Underyling data, APVI data, region and date of event


### Step 1, filter out sites containing circuits with not enough data or NA

# These are the categories we want to remove
BadCategories <- c("Not enough data", "Undefined")
# Select distinct site_id/response_category combos
BadSiteIds <- group_by(UnderlyingData, site_id, compliance_status) %>% summarise()
# Filter to get a list of site_ids to remove
# Note that R reads in the NAs as actual NA values which is annoying. You do you R.
BadSiteIds <- filter(BadSiteIds,compliance_status %in% BadCategories | is.na(compliance_status))
# Remove bad site_ids. Note the ! negates the %in% operator to make in 'not in'
UnderlyingData <- filter(UnderlyingData,!site_id %in% BadSiteIds$site_id)


### Step 2, recategorise site compliance to call out ambiguous sites
# (that is, sites which contain circuits of more than one compliance class)
# This is necessary because capactiy factors are only able to be computed at site level.

# First count the number of distinct compliance classes per site id
SiteIds <- group_by(UnderlyingData, site_id, compliance_status) %>% summarise()
SiteIds <- group_by(SiteIds, site_id) %>% summarise(Count=n())
UnderlyingData <- left_join(UnderlyingData, SiteIds, by="site_id")
# For some reason I get a number representing each class instead of the name of the
# class if I don't specify as.character?? Maybe it's trying to save memory by creating
# a set of foreign keys??
UnderlyingData <- mutate(UnderlyingData, NewCompliance = 
                           ifelse(Count>1,"Ambiguous",as.character(compliance_status)) )

### Step 3, form standard-dependent compliance classes
# These should be as is for 2015, no classes for transition and 2005 inverters should
# consist of Ambiguous, Off at t0, Disconnect/Drop to Zero and 'Other'
Keep2005Classes <- c("Ambiguous","Off at t0","Disconnect/Drop to Zero")
UnderlyingData <- mutate(UnderlyingData, NewCompliance = 
                    ifelse(Standard_Version == "AS4777.3:2005",
                      ifelse(NewCompliance %in% Keep2005Classes,NewCompliance,"Ride through/Curtail")
                      , ifelse(Standard_Version == "Transition","Transition",NewCompliance)))

### Step 4, calculate average capacity factor for each class
CapFactors <- group_by(UnderlyingData, ts, Standard_Version, NewCompliance) %>% summarise(CapFactor=mean(site_performance_factor))
# We'll need this column as a join key later
CapFactors <- mutate(CapFactors, StdComplianceCombined = paste(Standard_Version, NewCompliance))

### Step 5, calculate the proportion each compliance class represents within each standard
Proportions <- group_by(UnderlyingData, Standard_Version, c_id, NewCompliance) %>% summarise()
Proportions <- mutate(Proportions, StdComplianceCombined = paste(Standard_Version, NewCompliance))
TotalPerStandard <- group_by(Proportions, Standard_Version) %>% summarise(Total=n())
Proportions <- group_by(Proportions, Standard_Version, StdComplianceCombined) %>% summarise(Count=n())
Proportions <- left_join(Proportions, TotalPerStandard, by = "Standard_Version")
Proportions <- mutate(Proportions, Proportion = Count/Total)

### Step 6, assign capacities for each class and upscale
# Assigning capacity for each standard
Cap2005MW <- sum(filter(APVICapacity,index == "2015-10-01", State == Region )$Capacity)/1000
CapTransitionMW <- sum(filter(APVICapacity,index == "2016-11-01", State == Region )$Capacity)/1000 - Cap2005MW
Cap2015MW <- sum(filter(APVICapacity,index == DateOfEvent, State == Region )$Capacity)/1000 - CapTransitionMW - Cap2005MW
# Multipling capacity by category proportion by category capactiy factor to obtain upscaled data
CapFactors <- left_join(CapFactors,Proportions,by="StdComplianceCombined")
# There are two copies of Standard_Version after joining so one of them has the suffix '.x'
CapFactors <- mutate(CapFactors, UpscaledMW = 
                       ifelse(Standard_Version.x == "AS4777.2:2015", Cap2015MW*CapFactor*Proportion,
                              ifelse(Standard_Version.x == "AS4777.3:2005", Cap2005MW*CapFactor*Proportion, CapTransitionMW*CapFactor*Proportion)))

# Step 7, Tidying up CapFactors Table for plotting

# Add a time only column
# Turn 'Transition Transition' into just 'Transition',
# merge Off at t0 and ambiguous across standards and replace 'compliance' with softer wording
CapFactors <- mutate(CapFactors
                     # Date/Time functions are my Kryptonite, don't judge me
                     ,Time = hms::as.hms(as.numeric(substr(ts,18,19))+as.numeric(substr(ts,15,16))*60+as.numeric(substr(ts,12,13))*3600)
                     ,StdComplianceCombined=
                      ifelse(StdComplianceCombined=='Transition Transition','Transition'
                      ,ifelse(StdComplianceCombined=='AS4777.2:2015 Off at t0','Off at t0'
                      ,ifelse(StdComplianceCombined=='AS4777.3:2005 Off at t0','Off at t0'
                      ,ifelse(StdComplianceCombined=='AS4777.2:2015 Ambiguous','Ambiguous'
                      ,ifelse(StdComplianceCombined=='AS4777.3:2005 Ambiguous','Ambiguous'
                      ,ifelse(StdComplianceCombined=='AS4777.2:2015 Non-compliant','AS4777.2:2015 Not responding'
                      ,ifelse(StdComplianceCombined=='AS4777.2:2015 Non-compliant Responding','AS4777.2:2015 Partially responding'
                      ,ifelse(StdComplianceCombined=='AS4777.2:2015 Compliant','AS4777.2:2015 Responding as specified'
                      ,StdComplianceCombined))))))))
                    )
# Now summarise to merge off at t0 and ambiguous
CapFactors <- group_by(CapFactors,StdComplianceCombined,ts,Time) %>% summarise(UpscaledMW = sum(UpscaledMW))
# Ordering StdComplianceCombined to give the stack order we want
# ggplot2 uses the order of the levels in a factor to order the stack
CapFactors <- mutate(CapFactors,Legend=factor(StdComplianceCombined,levels=c('Off at t0','Ambiguous','Transition'
                ,'AS4777.3:2005 Disconnect/Drop to Zero','AS4777.3:2005 Ride through/Curtail'
                ,'AS4777.2:2015 Disconnect/Drop to Zero','AS4777.2:2015 Responding as specified'
                ,'AS4777.2:2015 Partially responding','AS4777.2:2015 Not responding')))
# This will determine how dense the x-axis time values are on the plot
XAxisBreaks <- group_by(CapFactors,Time) %>% summarise()
# Edit to reflect how many minutes between each x-axis label. 2 is good for an hour of data.
BreaksBetween <- 2
XAxisBreaks <- filter(XAxisBreaks,as.numeric(substr(as.character(XAxisBreaks$Time),4,5))%%BreaksBetween==0)

# This will save the plot in high resolution at the specified dimensions in your directory
tiff(paste(DateOfEvent,Region,"upscaled response.tiff"), units="in", width=8, height=4, res=300)

p <- ggplot(CapFactors, aes(Time, UpscaledMW,fill=Legend))+
  geom_area(position="stack")+
  theme(text=element_text(family="Segoe UI Semilight", colour = "#767171"),legend.position="right"
        ,axis.text.x = element_text(angle = 90, hjust = 1, size = 8, colour = "#767171"))+
  scale_fill_manual( #That's right, I converted the default excel colours from RGB to Hex
                      # 1 by 1 because I''m nothing if not consistent
                    values=c("#3B3838","#767171","#D0CECE","#4472C4","#44546A"
                             ,"#E2F0D9","#A9D18E","#548235","#385723")) +
  scale_x_time(breaks=XAxisBreaks$Time) +
  xlab("Time")+
  ylab("Distributed PV generation (MW)")


plot(p)

# This I think tells the Tiff line of code that we're done with the plot
dev.off()

# And another file that shows each category on its own
tiff(paste(DateOfEvent,Region,"upscaled response split.tiff"), units="in", width=8, height=4, res=300)

# This will determine how dense the x-axis time values are on the plot
XAxisBreaks <- group_by(CapFactors,Time) %>% summarise()
# Edit to reflect how many minutes between each x-axis label.
BreaksBetween <- 6
XAxisBreaks <- filter(XAxisBreaks,as.numeric(substr(as.character(XAxisBreaks$Time),4,5))%%BreaksBetween==0)

p <- p + facet_wrap(~Legend) + scale_x_time(breaks=XAxisBreaks$Time)

plot(p)

dev.off()