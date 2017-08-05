
#--------------

rm(list = setdiff(ls(), lsf.str()))
dropboxroot <- "C:/Users/Udit_Gupta/Dropbox/"
OUTPUT <- paste(dropboxroot,"ETS CEMS GJ/0. Recalibration 2017/15.Output/Analysis/if all pass isok/",sep="")


#--check which results 




list <- import_googlesheet()
list_df <- as.vector(unique(list$Composite.ID))

#write.csv(file=paste(dropboxroot,"List2.csv",sep=""),list_df)



#--------Industry PC----
   
    rm(list = setdiff(ls(), lsf.str()))
    dropboxroot <- "C:/Users/Udit_Gupta/Dropbox/"
    OUTPUT <- paste(dropboxroot,"ETS CEMS GJ/0. Recalibration 2017/15.Output/Analysis/",sep="")
    
    
    for (i in 9:9) {
   #   i <-7
      dropboxroot <- "C:/Users/Udit_Gupta/Dropbox/"
      OUTPUT <- paste(dropboxroot,"ETS CEMS GJ/0. Recalibration 2017/15.Output/Analysis/",sep="")
      
      #composite_id <-list_df[i]
      composite_id <- "GJSRT208480111"

      options(warn=1)
      check_result()
      sample_result_df<- make_sample_result_df()
      ind_result_df <- make_ind_result_df_industrypc()
      df_result <- import_result()
      data_raw_csv  <- read_raw_CSV()
      
      data_raw_csv <- identify_rawdatawithsample()
      data_raw_csv <- pcdata_compile()
      data_av <- calculate_data_availabilty()
      df_analysis <- merge_rawPM()
      df_analysis <- createmsg()
      df_analysis <- calculate_PMload()
      #df_analysis <- df_analysis[,c(1:18,20:24)]
      #df_analysis <- add_zero_reading()
      ind_result_df <- calculate_calib_factors()
      modifycsv_rawfromPC()
      #beepr::beep(8)
      t <- setdiff(ls(), lsf.str())
      t1 <- setdiff(t, "i")
      t2 <- setdiff(t, "list_df")
      rm(list = t2)
      
     # rm(list = setdiff(ls(), lsf.str(mode = "function")))
    }
    
    
    
    hist()
    #---------------------
    #------Glens--------------------------
    rm(list = setdiff(ls(), lsf.str()))
    dropboxroot <- "C:/Users/Udit_Gupta/Dropbox/"
    
    t <- setdiff(ls(), lsf.str())
   
    
    
    check_result()
    sample_result_df<- make_sample_result_df()
    ind_result_df <- make_ind_result_df_glens()
    df_result <- import_result()
    data_raw_csv  <- read_raw_CSV()
    
    data_raw_csv <- identify_rawdatawithsample()
    data_av <- calculate_data_availabilty()
    df_analysis <- merge_rawPM()
    df_analysis <- createmsg()
    ind_result_df <- calculate_calib_factors()
    modifycsv_rawfromGLens()
    
    
    #----------------combine aggregreat resuults---------
    
    list <- c("GJSRT204800111", "GJSRT205190111", "GJSRT209680111", "GJSRT210560111", "GJSRT208150111", "GJSRT206320311", "GJST1204900111", "GJSRT208890111", "GJSRT215080111")
    list2 <- c("GJSRT209400111","GJSRT210950111","GJSRT205910111")
    
    for (i in 1:3) {
      composite_id <-list2[i]
      df_result <- import_result()
      #df_result_combined <- df_result
      df_result_combined <- import_result_combine()
    }
    
    #----plotting sampling volumes--------------
    par(mar = rep(2, 4))
    hist(df_result_combined$`Total gas passed (litres)`,ylab="No of Samples",
         main="Distribution of Volume of Gas Sampled",
         xlab="Volume in Litres")
    plot_volume <-  ggplot(df_result_combined,aes(x=`Total gas passed (litres)`))+
      geom_histogram(binwidth = 50)+theme_fivethirtyeight()+
      ylab("No of Samples") +
      ggtitle("Distribution of Volume of Gas Sampled") +
      xlab(label="Sampled Volume in Litres") +
      theme(legend.position = "bottom", legend.direction = "horizontal",
            legend.box = "horizontal",
            legend.key.size = unit(1, "cm"),
            plot.title = element_text(family="Tahoma"),
            text = element_text(family = "Tahoma"),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 9),
            legend.title=element_text(face = "bold", size = 9))
    
    ggsave("C:/Users/Udit_Gupta/Dropbox/ETS CEMS GJ/0. Recalibration 2017/15.Output/Histogram sampling volume.png",plot_volume,width=8.5,height = 6.5)
    
    #----plotting Load vs --------------
    require(ggthemes)
    library(ggthemes)
   colnames(df_result_combined)[15] <- "PM Sampled Concentration (mg/Nm3)"
    
    df_plot_high <- subset(df_result_combined,Load_Category=="High") 
    df_plot_low <- subset(df_result_combined,Load_Category=="Low") 
    
    p1 <- ggplot(df_plot_high,aes(x=`PM Sampled Concentration (mg/Nm3)`))+
      geom_histogram(binwidth = 50)+theme()+
      scale_x_continuous(limits = c(0, 3000),breaks = seq(0,3000,500))+
      scale_y_continuous(breaks = seq(0,20,5))+
      ylab(label="No of Samples") +
      ggtitle("High Load Category")
    
   ggsave("C:/Users/Udit_Gupta/Dropbox/ETS CEMS GJ/0. Recalibration 2017/15.Output/Histogram PM Readings High Load.png",p1,width=8.5,height = 6.5)
    
   p2 <- ggplot(df_plot_low,aes(x=`PM Sampled Concentration (mg/Nm3)`))+
     geom_histogram(binwidth = 50)+theme()+
     scale_x_continuous(limits = c(0, 2000),breaks = seq(0,2000,500))+
     scale_y_continuous(breaks = seq(0,20,5))+
     ylab(label="No of Samples") +
     ggtitle("Low Load Category")
   
  ggsave("C:/Users/Udit_Gupta/Dropbox/ETS CEMS GJ/0. Recalibration 2017/15.Output/Histogram PM Readings Low Load.png",p2,width=8.5,height = 6.5)
  
  df_result_combined_copy <- df_result_combined
  df_result_combined_copy$Load_Category=as.numeric(levels(df_result_combined_copy$Load_Category))[df_result_combined_copy$Load_Category]
  
  means <- aggregate(df_result_combined_copy$`PM Sampled Concentration (mg/Nm3)`~ df_result_combined_copy$Load_Category, mean)
  
 p3<- ggplot(df_result_combined_copy, aes(x=as.factor(df_result_combined_copy$Load_Category), y=df_result_combined_copy$`PM Sampled Concentration (mg/Nm3)`)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    scale_y_continuous(limits = c(0, 1500),breaks = seq(0,1500,150))+
    ylab(label="PM Measured Concentration (mg/Nm3)") +
    xlab("Load Category")+
    ggtitle("PM Concentration vs Load Category")+
   geom_hline(yintercept = 150)
    
    
    ggsave("C:/Users/Udit_Gupta/Dropbox/ETS CEMS GJ/0. Recalibration 2017/15.Output/Box Plot PM Readings vs Load.png",p3,width=8.5,height = 10.5)
    
   
    
    
    
    #-----------------------------
    
#rmarkdown::render("MyDocument.Rmd", params = "ask")

rmarkdown::render("C:/Users/Udit_Gupta/Dropbox/ETS/Data/recalibration_2017/Code/recalibration/recalibration output.Rmd",output_file = paste(dropboxroot,"ETS CEMS GJ/0. Recalibration 2017/15.Output/",'report.', composite_id, '.pdf', sep=''),params = "ask")

    
    
    which(is.na(df2$G_Time))
    