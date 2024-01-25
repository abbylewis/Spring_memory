#'This code file contains functions needed for the regression analyses in "05 - Memory driver analysis.Rmd"
#'
#'Function list
#'
#'  zscore()
#'  na_to_zero()
#'  standardize_data()
#'  renamer()


###
#Utilities
###

#Calculate the zscore of data in a vector
zscore = function(data, na.rm = T){(data-mean(data, na.rm = na.rm))/sd(data, na.rm = na.rm)}
#Sets NAs to 0
na_to_zero = function(vector){ifelse(is.na(vector),0,vector)}
#Standardize data
standardize_data = function(dataset, responses, potential_drivers){
  dataset = dataset%>%
    ungroup()%>%
    dplyr::select(all_of(c(potential_drivers,responses,"LakeID")))%>%
    filter(if_all(where(is.numeric),is.finite))%>%
    mutate(across(-all_of(c("LakeID", responses)),zscore))
  return(dataset)
}

###
#Renamer
###

#Rename variables in paper figures
renamer = function(text_vect){
  output=text_vect
  i = 1
  while(i <= length(text_vect)){
    text = text_vect[i]
    if(text=="log_max_depth")      output[i]="Maximum depth"
    if(text=="log_SA")             output[i]="Surface area"
    if(text=="Latitude_DD")        output[i]="Latitude"
    if(text=="buoyancy_freq")      output[i]="Maximum\nbuoyancy freq."
    if(text=="log_SA:log_max_depth") output[i]="Surface area :\nmaximum depth"
    i = i+1
  }
  return(output)
}