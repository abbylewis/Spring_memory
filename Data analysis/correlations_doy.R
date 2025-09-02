#' Daily correlations
#'
#' @param wi_lakes_all_data data frame
#' @param variable variable to plot
#' @param value value to correlate with (e.g., air temperature)
#' @param name name for y axis
#' @param partial should partial correlations be used? 
#'

correlations_doy <- function(wi_lakes_all_data, 
                             variable, 
                             value, 
                             name, 
                             partial = T) {
  #Parse variable name
  variable <- sym(variable)
  value <- sym(value)
  
  many_lake_stat1 <- wi_lakes_all_data %>%
    dplyr::select(LakeID, doy, Year, !!variable, !!value) %>%
    filter(!is.na(!!variable), !is.na(!!value), doy < yday("2022-08-31")) %>%
    group_by(LakeID, doy) %>%
    mutate(nyear = length(unique(Year))) %>%
    filter(nyear>=15,
           #max(value)-min(value)>2
    ) %>%
    dplyr::select(LakeID, doy, Year, !!variable, !!value, nyear)
  
  many_lake_stat <- purrr::map(unique(many_lake_stat1$LakeID), 
             daily_cors, 
             data = many_lake_stat1, 
             variable = variable, 
             value = value,
             partial = partial,
             .progress = "Running many lakes: ") %>%
    list_rbind()
  
  wilcox <- many_lake_stat%>%
    group_by(doy)%>%
    mutate(p = wilcox.test(monthly_correlation)$p.value,
           n = n())
  return(wilcox)
}


daily_cors <- function(lake, data, variable, value, partial) {
  if(partial){
    result <- data %>%
      filter(LakeID == lake) %>%
      group_by(LakeID, doy) %>%
      summarize(monthly_correlation = ppcor::pcor.test(!!variable, !!value, 
                                                       Year, method = "spearman")$estimate,
                nyear = unique(nyear),
                .groups = "drop")
  } else {
    result <- data %>%
      filter(LakeID == lake) %>%
      group_by(LakeID, doy) %>%
      summarize(monthly_correlation = cor(!!variable, !!value, 
                                          method = "spearman"),
                nyear = unique(nyear),
                .groups = "drop")
  }
  return(result)
}
