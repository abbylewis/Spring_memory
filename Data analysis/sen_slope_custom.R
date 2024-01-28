split_year = 1800

sen_slope_custom <- function(df,var){
  output = df%>%
    group_by(LakeID)%>%
    dplyr::summarize(n = n(),
                     trend = NA,
                     sig = NA,
                     min_year = NA,
                     max_year = NA)
  for(lake in unique(df$LakeID)){
    filt = df %>%
      filter(LakeID==lake) %>%
      mutate(date = as.POSIXct(paste0(Year,"-01-01"))) %>%
      ungroup()%>%
      dplyr::select(-Year)
    if(length(unique(year(filt$date)))>=10){#Only calculate a trend if there are 10 years of data
      sen = trend::sens.slope(filt[[var]])
      output$trend[output$LakeID==lake]<-sen$estimates[1]
      output$sig[output$LakeID==lake]<-sen$p.value[1]
      output$min_year[output$LakeID==lake]<-min(year(filt$date))
      output$max_year[output$LakeID==lake]<-max(year(filt$date))
      output$var <- var
    }
  }
  return(output)
}

#Function to calculate sen slope for a given day
sen_slope_climate <- function(df,var){
  df <- df %>%
    mutate(date = ymd_hms(paste(Year,"-01-01 00:00:00")))
  output = df %>%
    group_by(LakeID)%>%
    dplyr::summarize(n = n(),
                     trend = NA,
                     sig = NA,
                     min_year = NA,
                     max_year = NA)
  for(lake in unique(df$LakeID)){
    filt = df%>%
      filter(LakeID==lake)
    if(length(unique(filt$Year))>=5){
      sen = trend::sens.slope(filt[[var]])
      output$trend[output$LakeID==lake]<-sen$estimates[1]
      output$sig[output$LakeID==lake]<-sen$p.value[1]
      output$min_year[output$LakeID==lake]<-min(year(filt$date))
      output$max_year[output$LakeID==lake]<-max(year(filt$date))
    }
  }
  return(output)
}

#Function to run for every day of the year
sen_slope_doys <- function(df, var, doys) {
  output <- sen_slope_climate(df%>%filter(doy == doys[1]), var)%>%
    mutate(doy = doys[1])
  
  if(length(doys)>1){
    for (doy_n in doys[2:length(doys)]) {
      output <- rbind(output, 
                      sen_slope_climate(df %>% filter(doy == doy_n), var)%>%
                        mutate(doy = doy_n)
      )
    }
  }
  return(output)
}
