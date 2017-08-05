# Purpose: Recalibration Checks
# Author: Udit Gupta
# Date created: 2017-06-25

#------------------------0. House Keeping------------------
rm(list = ls())

# Set dropbox directory
if(Sys.info()[['sysname']]=="Windows") {  
  dropboxroot <- paste0("C:/Users/",Sys.info()[['user']],"/Dropbox/")
} else{
  if(Sys.info()[['sysname']]=="Darwin"){    
    dropboxroot <- paste0("/Users/",Sys.info()[['user']],"/Dropbox/")
  }
}



# Load required packages

# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("knitr","plyr","dplyr", "lubridate", "readr", "data.table","bit64","reshape2","ggplot2","readxl","scales","R.utils","googlesheets","magrittr","MLmetrics","kableExtra","xtable","rmarkdown","qdap")
ipak(packages)
#options(scipen = 999)


rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}



OUTPUT <- paste(dropboxroot,"ETS CEMS GJ/0. Recalibration 2017/15.Output/Analysis/",sep="")









#----------1. Check which industries have been sampled---- 
#should have a date option #hash put in wrong places

import_googlesheet <- function() {
  
  suppressMessages(library(dplyr))
  #List all google sheets associated with IFMR Google ID--
  my_sheets <- gs_ls()
  calib_gs<- gs_title("Calibration File 2017")
  #calib_gs <- as.data.frame(calib_gs)
  calib_gs_post<- calib_gs %>%
    gs_read(ws = "Post-Calibration")
  
  colnames(calib_gs_post) = calib_gs_post[1, ] # the first row will be the header
  calib_gs_post <- calib_gs_post[-1, ] 
  
  
  
  
  #df <- paste(calib_gs_post$Composite.ID,calib_gs_post$)
  
  #----subset for fully completed industries
  calib_gs_post <- calib_gs_post[which(calib_gs_post$L4.Report=="Received"),]
  calib_gs_post <- calib_gs_post[which(calib_gs_post$Status=="Completed"),] # | calib_gs_post$Status=="Zero load left"),]
 
  #listofid_results <- calib_gs_post$composite_id
  #print(These industries were calibrated in the last week )
  #return(unique(calib_gs_post$Composite.ID))
  return(calib_gs_post)
  
}


#--2. Check if excel results are there-------

check_result <- function()  {
  
  meta <- read.csv(paste(dropboxroot,"ETS CEMS GJ/2. Data/Meta/sample_list.csv",sep=""))
  setwd(paste0(dropboxroot,"ETS CEMS GJ/0. Recalibration 2017/14. Lab Results/",sep=""))
  folders <- list.dirs('.', recursive=FALSE)
  #folder_week <- folder_week[week_index_range]
  root <- getwd()
  ind_id <- list.dirs(path = paste0(root),recursive = F)
  filenames_full <- list.files(path = ind_id,pattern="\\.xlsx$",full.names = TRUE)
  filenames <- list.files(path = ind_id,pattern="\\.xlsx$") 
  filenames <- substr(filenames,01,14)
  df <- as.data.frame(filenames)
  
  message <- ifelse(composite_id %in% filenames,"Result Present","Result Not Present")
  #colnames(df)[1] <<- "Composite ID"
  return(message)
  
}  



#---3. Create Dataframe to compile results=======

make_sample_result_df <- function() {
  
  # readLine("Enter Compoposite ")
  #composite_id <<- composite_id
  sample_result_df <- as.data.frame(rep(composite_id,8))
  sample_result_df[,2] <- 1:8
  colnames(sample_result_df)[1:2] <- c("composite_id","sample no.")
  return(sample_result_df)
  
  
}

make_ind_result_df_glens <- function() {
  
  ind_result_df <- read.csv(paste(OUTPUT,"Overall Results",".csv",sep=""),stringsAsFactors = F)
  return(ind_result_df)
  
}

make_ind_result_df_industrypc <- function() {
  
  ind_result_df <- read.csv(paste(OUTPUT,"Overall Results",".csv",sep=""),stringsAsFactors = F)
  return(ind_result_df)
  
}

#---4. import results in df_result-
  #look at 170----------

import_result <- function() {
  
  #composite_id <- "GJSRT205190111"
  message <- check_result()
  stopifnot(message=="Result Present")
  #print("Result Present Importing excel result...")
  
  meta <- read.csv(paste(dropboxroot,"ETS CEMS GJ/2. Data/Meta/sample_list.csv",sep=""))
  setwd(paste0(dropboxroot,"ETS CEMS GJ/0. Recalibration 2017/14. Lab Results/",sep=""))
  folders <- list.dirs('.', recursive=FALSE)
  #folder_week <- folder_week[week_index_range]
  root <- getwd()
  ind_id <- list.dirs(path = paste0(root),recursive = F)
  filenames_full <- list.files(path = ind_id,pattern="\\.xlsx$",full.names = TRUE,all.files=FALSE)
  filenames_full <- as.data.frame(filenames_full)
  filenames_full$filenames_full <- as.character(filenames_full$filenames_full)
  #filenames_full <- filenames_full[!grep("~$", filenames_full$filenames_full),]
  
  #filenames_full$filenames <- list.files(path = ind_id,pattern="\\.xlsx$")
  #filenames <- filenames[!grepl("~$", filenames)]

#  filenames_full$filenames <- substr(filenames,01,14)
 # filenames_full$t <- substr(filenames,01,02)
  #filenames <- filenames[grepl("GJ",t)]
  #filenames_full <- filenames_full[grepl("GJ",t)]
  #full_path <- filenames_full[match(composite_id,filenames)]
  full_path <- filenames_full$filenames_full[grepl(composite_id,filenames_full$filenames_full)]
  full_path <- full_path[length(full_path)]
  
    library(dplyr)
  
  df_result <- read_excel(full_path,sheet=1)
  #select only for Date of Sampling after 08 June.
  
  df_result <- tbl_df(df_result)
  df_result$Load_Category <- ifelse(df_result$`% Load on the process/ emissions`==0,"Zero",(ifelse(df_result$`% Load on the process/ emissions`<75,"Low","High")))
  df_result <- subset(df_result, (df_result$`Date of Sampling` > as.POSIXct("2017-06-08",tz="UTC")))
  
   
  return(df_result)
  
  #stopifnot(nrow(df_result)>5)
  print(paste("There are results for",nrow(df_result),"samples",sep=" "))
  print("Results Imported Succesfully")
  
}  


#---6. imports raw weekly csv from glens into data_raw_csv  
#this is the data of the whole day of sampling-------

read_raw_CSV <- function() {
  
  #----Find filenames to find--
  library(magrittr)
  
  date_of_sampling <- as.data.frame(unique(df_result$`Date of Sampling`))
  d <- format(date_of_sampling,format="")
  
  filenames_to_find <- d %>% 
    mutate_all(funs(gsub("-","",.))) %>%
    set_colnames(c("DateofSampling")) %>%
    mutate(compositeid=rep(composite_id,nrow(d))) %>%
    mutate(filename_to_find = paste(`DateofSampling`,`compositeid`,sep="_")) %>%
    select(filename_to_find)
  
  
  #----Read filenames to find from Weekly CSVs-
  ROOT<- paste0(dropboxroot,"CEMS G-Lens/Data/CSV")
  setwd(ROOT)
  folder_week <- list.dirs('.', recursive=FALSE)
  ind_id <- list.dirs(path = paste0(ROOT,folder_week),recursive = F)
  ind_id <- ind_id[which(grepl(composite_id,ind_id))]
  filenames_full <- list.files(path = ind_id,pattern="\\.csv$",full.names = TRUE)
  #filenames <- list.files(path = ind_id,pattern="\\.csv$") 
  
  matching_filenames <- which(grepl(paste(filenames_to_find$filename_to_find,collapse = "|"),filenames_full))
  files_to_read <- filenames_full[matching_filenames]
  
  df_raw <-lapply(files_to_read, function(x) read.csv(x, stringsAsFactors = FALSE,skip=2,header = F))
  
  
  #---Format dataframe from rAw CSVs
  
  #Format Dataframe to keep only columns needed
  df2 <- ldply(df_raw, data.frame)
  df2 <- df2[!grepl("eof", df2$V1),] 
  df2 <- df2[,1:3]
  
  colnames(df2)[2] <- "G_PM_Uncal"
  colnames(df2)[3] <- "G_PM_Cal"
  colnames(df2)[1] <- "G_Time"
  
  df2$G_Time <- ymd_hms(df2$G_Time,tz="Asia/Calcutta")
  df2$G_Time <- floor_date(df2$G_Time,"minute")
  #Add industry ID
  df2$indid <- rep(composite_id,nrow(df2))
  data_raw_csv <-df2[,c(4,1,2,3)]
  data_raw_csv$sample_no <- 0
  return(data_raw_csv)
  print("Raw Data Imported Successfully")
  
}


#-----Put Sample no in raw data

identify_rawdatawithsample <- function(){
  #rename two columns for start and end time
 # df_result <- df_result %>%
  #  rename("Start Time" = `Start Time of sampling (HHMM)`)
  
  colnames(df_result)[10:11] <- c("Start Time","End Time")
  
  #extract only the tiem from start and end time columns and combine with date of sampling 
  
  df_result$S_starttime <- as.POSIXct(paste(df_result$`Date of Sampling`, strftime(df_result$`Start Time`, format="%H:%M:%S",tz="UTC")),format="%Y-%m-%d %H:%M:%S")
  df_result$S_endtime <- as.POSIXct(paste(df_result$`Date of Sampling`, strftime(df_result$`End Time`, format="%H:%M:%S",tz="UTC")), format="%Y-%m-%d %H:%M:%S")
  
  #insert a column of sample no which will sample number corresponding to the minute
  
  t <-nrow(df_result)
  
  for (i in 1:t) {
    
    data_raw_csv[which(data_raw_csv$G_Time>=df_result$S_starttime[i] & data_raw_csv$G_Time<=df_result$S_endtime[i]),5] <-i
    
  }
  
  return(data_raw_csv)
  
}

#---7. Check if 95 % of it is available====

calculate_data_availabilty <- function() {
  
  #int <- interval(data_raw_csv$G_Time,data_raw_csv$)
 # detachPackage(plyr)
  
  
  raw_mean_da <- aggregate(G_PM_Uncal ~ sample_no, data=data_raw_csv, function(x) {mean(!is.na(x))}, na.action = NULL)
  colnames(raw_mean_da)[2] <- "raw_mean_da"
  
  raw_mean <- aggregate(G_PM_Uncal ~ sample_no, data=data_raw_csv, function(x) {mean(x,na.rm=T)}, na.action = NULL)
  colnames(raw_mean)[2] <- "raw_mean_value"
  
  
  data_av <- left_join(raw_mean_da,raw_mean)
  
  data_av <- data_av %>% 
    filter(sample_no > 0)
  # stopifnot("D")
  #---Stop if any sample is 
  return(data_av)
  #start storing it in a 8-row matrix-- form of output--
}

#---create df_analysis and merge data_av ------

merge_rawPM <- function() {
  
  
  #create a copy of df_result
  
  df_analysis <- df_result
  colnames(df_analysis)[15] <- "PM_sampled"

  
  colnames(df_analysis)[which(names(df_analysis) == "Sample No.")] <- "sample_no"
  
  df_analysis <- left_join(df_analysis,data_av)
  # colnames(df_analysis)[]
  return(df_analysis)
  
}



#-------create message for data av and isok, add zero, zero 

createmsg <- function(){
  
  df_analysis$Isokinetic_check <- ifelse(df_analysis$`Isokineticity (%)`>=90 & df_analysis$`Isokineticity (%)`<=110,"Pass","Fail")
  #df_analysis$Isokinetic_check <- ifelse(df_analysis$`Isokineticity (%)`<=10,"Pass","Fail")
  
  df_analysis$`Data Availaility Check` <- ifelse(df_analysis$raw_mean_da >=0.95,"Pass" ,"Fail") 
  #df_analysis[,nrow+1] <- df_analysis %>%
  #t <- as.data.frame( df_analysis[nrow(df_analysis),] )
  #df_analysis <- rbind.data.frame(t,df_analysis)
  return(df_analysis)
}


#---------Calculate Normalised Flow and PM in kg/hr

calculate_PMload <- function() {
  
  df_analysis <- df_analysis %>%
    mutate(normalised_flow = `Flue Gas Velocity (m/s)`*3.14*`Stack Diameter (in meter)`*`Stack Diameter (in meter)`*273.15/ (`Temp (°C)`+273) * `Static Pressure (kPa)`/ 1.01325) %>%
    mutate(PMload = PM_sampled*normalised_flow*3600/1000000)
  return(df_analysis)
}

#---------add zero reading if not present------------

add_zero_reading <- function(){
  
  
  if ("Zero" %in% df_analysis$Load_Category) {
    
    return(df_analysis) 
        
  } else {
      norow <-nrow(df_analysis)
      df_analysis[norow,8]  <-  df_analysis[(norow-1),8]+1
      df_analysis[norow,15] <- as.data.frame(0)
      df_analysis[norow,18] <- as.data.frame(0)
      df_analysis[norow,21] <- as.data.frame(0)
      df_analysis[norow,19] <- "Zero"
      df_analysis[norow,22:23] <- "Pass"
      return(df_analysis)
  }
  
}



#----calculate calib factors--------

calculate_calib_factors <- function() {
    
    df_analysis <- df_analysis[,2:25]
    no_of_samples_isok_pass <- sum(df_analysis$Isokinetic_check == "Pass")
    no_of_samples_da_pass <- sum(df_analysis$`Data Availaility Check` == "Pass")
    no_of_samples_isok_fail <- sum(df_analysis$Isokinetic_check == "Fail")
    no_of_samples_da_fail <- sum(df_analysis$`Data Availaility Check`== "Fail")
    no_of_samples_zeroload <- sum(df_analysis$Load_Category == "Zero")
    no_of_samples_highload <- sum(df_analysis$Load_Category == "High")
    no_of_samples_lowload <- sum(df_analysis$Load_Category == "Low")
    no_of_samples_zeroload_invalid <- sum(df_analysis$Load_Category == "Zero" & (df_analysis$Isokinetic_check=="Fail" | df_analysis$`Data Availaility Check` =="Fail"))
    no_of_samples_highload_invalid <- sum(df_analysis$Load_Category == "High" & (df_analysis$Isokinetic_check=="Fail" | df_analysis$`Data Availaility Check` =="Fail"))
    no_of_samples_lowload_invalid <- sum(df_analysis$Load_Category == "Low" & (df_analysis$Isokinetic_check=="Fail" | df_analysis$`Data Availaility Check` =="Fail"))
    
    
    #df_analysis <- df_analysis[,2:23]
    
    df_analysis <- df_analysis %>% 
     filter(Isokinetic_check=="Pass" & `Data Availaility Check`=="Pass")
    
    df_analysis <- df_analysis %>% 
      filter(`Data Availaility Check`=="Pass")
    
    no_of_valid_highload_samples <- df_analysis %>% 
      filter(Load_Category=="High") %>%
      nrow()
    
    no_of_valid_zeroload_samples <- df_analysis %>% 
      filter(Load_Category=="Zero") %>%
      nrow()
    
    if(no_of_valid_highload_samples>=4 & no_of_valid_zeroload_samples==1) {
    
    model <-lm(PMload~raw_mean_value,data=select(df_analysis,PMload,raw_mean_value))
   # model <-lm(df_analysis$PMload~df_analysis$raw_mean_value,data=df_analysis)
    
    rmspe <- RMSPE(y_pred = model$fitted.values[1:(length(model$fitted.values)-1)],y_true = df_analysis$PMload[1:(length(df_analysis$PMload)-1)])*100
    c<-  model$coefficients[1]
    m<- model$coefficients[2]
    
    valid_calib_range <- 1.20 * (m*max(df_analysis$raw_mean_value)+c)
  
    ind_result_df$rmspe_check[which(ind_result_df$composite_id==composite_id)] <- ifelse(rmspe <=30,"Pass" ,"Fail")
    ind_result_df$rmspe[which(ind_result_df$composite_id==composite_id)] <- rmspe
    ind_result_df$m[which(ind_result_df$composite_id==composite_id)] <- ifelse(rmspe <=30, m ,NA)
    ind_result_df$c[which(ind_result_df$composite_id==composite_id)] <- ifelse(rmspe <=30, c ,NA)
    ind_result_df$valid_calib_range[which(ind_result_df$composite_id==composite_id)] <- ifelse(rmspe <=30, valid_calib_range ,NA)
    
    } else {
      
    ind_result_df$rmspe_check[which(ind_result_df$composite_id==composite_id)] <- "Insufficient valid samples"
   
    }
    
   # ind_result_df <- ind_result_df[, -grep("X", colnames(ind_result_df))]
    
    ind_result_df$No.of.Isokinetic.Samples[which(ind_result_df$composite_id==composite_id)] <- no_of_samples_isok_pass
    ind_result_df$No.of.non.isokinetic.samples[which(ind_result_df$composite_id==composite_id)] <- no_of_samples_isok_fail
    ind_result_df$No.of.Samples.with.DA[which(ind_result_df$composite_id==composite_id)] <- no_of_samples_da_pass
    ind_result_df$No.of.DA.fail.samples[which(ind_result_df$composite_id==composite_id)] <- no_of_samples_da_fail
    ind_result_df$No.of.zero.load.samples[which(ind_result_df$composite_id==composite_id)] <-  no_of_samples_zeroload
    ind_result_df$No.of.high.load.samples[which(ind_result_df$composite_id==composite_id)] <-  no_of_samples_highload
    ind_result_df$No.of.low.load.samples[which(ind_result_df$composite_id==composite_id)] <-  no_of_samples_lowload
    ind_result_df$No.of.zero.load.invalid.samples[which(ind_result_df$composite_id==composite_id)] <-  no_of_samples_zeroload_invalid
    ind_result_df$No.of.high.load.invalid.samples[which(ind_result_df$composite_id==composite_id)] <-  no_of_samples_highload_invalid
    ind_result_df$No.of.low.load.invalid.samples[which(ind_result_df$composite_id==composite_id)] <-  no_of_samples_lowload_invalid
    
    return(ind_result_df)
  
}

#---Modify CSVs

modifycsv_rawfromGLens <- function() {
  
  write.csv(ind_result_df,file=paste(dropboxroot,"ETS CEMS GJ/0. Recalibration 2017/15.Output/Glens Data/","Overall Results",".csv",sep=""),row.names=FALSE)
  write.csv(df_analysis,file=paste(dropboxroot,"ETS CEMS GJ/0. Recalibration 2017/15.Output/Glens Data/",composite_id,"-",df_analysis$`Industry Name`[1],".csv",sep=""),row.names=FALSE)
  
}


#---Modify CSVs

modifycsv_rawfromPC <- function() {
  
  write.csv(ind_result_df,file=paste(OUTPUT,"Overall Results",".csv",sep=""),row.names=FALSE)
  write.csv(df_analysis,file=paste(OUTPUT,composite_id,"-",df_analysis$`Industry Name`[1],".csv",sep=""),row.names=FALSE)
  print(df_analysis$`Industry Name`[1])
}

  #df_analysis$PM_sampled <- c(1,2,3,4,5,6,7,10)
 

#8. call update

#sum(calib_gs_post$`Number of Samples taken`)
#sum(as.numeric(calib_gs_post$`Number of Samples taken`),na.rm = T)
#sum(calib_gs_post$Status)
#table(calib_gs_post$Status)

#----Read raw data collected manually

#-----Fetch Data from PC -----------------------------


pcdata_compile <- function() {
  #set root to csv containing folder
  ROOT<- paste0(dropboxroot,"ETS CEMS GJ/0. Recalibration 2017/11.Raw CSV/",sep="")
  #setwd(ROOT)
  
  #List files to look for
  ind_id <- list.dirs(path = ROOT,recursive = F)
  filenames_full <- list.files(path = ind_id,full.names = TRUE)
  filenames <- list.files(path = ind_id,pattern="\\.csv$") 
  #ff_full <- as.data.frame(filenames_full)
  #ff <- as.data.frame(filenames)
  filestolookfor <- paste(df_result$`Date of Sampling`,df_result$`Composite ID`,sep="_")
  filestolookfor <- filestolookfor[!duplicated(filestolookfor)]
 # filestolookfor <- filestolookfor[!which(grepl("NA",filestolookfor))]

  filestolookfor <-mgsub("-","",filestolookfor)
  t <- length(filestolookfor) 
  X <- vector(mode="character", length=t)
 #find index of these files
  for (i in 1:t) {
    X[i] <- filenames_full[grepl(filestolookfor[i],filenames_full)]
  }
  
  filenames_full <-X
  #Import files looking for
  
  df<-lapply(filenames_full, function(x) read.csv(x, stringsAsFactors = FALSE,skip=2,header = F))
  df2 <- ldply(df, data.frame)
  df2 <- df2[!grepl("eof", df2$V1),] 
  df2 <- df2[,1:2]
  
  df2$V1<-ymd_hms(df2$V1,tz="Asia/Calcutta")
  colnames(df2)[1] <- "G_Time"
  df2$G_Time <- floor_date(df2$G_Time,"minute")
  
  
  #format as glens csv
  df_format <- data_raw_csv
  #df_format <- df_format 
  df_format <- left_join(df_format,df2)
  df_format$G_PM_Uncal <- df_format$V2
  df_format <- df_format[1:5]
  
  data_raw_csv <- df_format
  return(data_raw_csv)
  
}


  