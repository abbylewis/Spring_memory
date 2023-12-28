#' Monthly correlations
#'
#' @param wi_lakes_all_data data frame
#' @param variable variable to plot
#' @param value value to correlate with (e.g., air temperature)
#' @param name name for y axis
#' @param alpha alpha value for significance
#'
#' @return
#' @export
#'
#' @examples
monthly_correlations <- function(wi_lakes_all_data, variable, value, name, alpha = 0.001) {
  #Parse variable name
  variable <- sym(variable)
  value <- sym(value)
  
  many_lake_stat <- wi_lakes_all_data %>%
    filter(!is.na(!!variable), !is.na(mon), !is.na(!!value))%>%
    group_by(LakeID, mon)%>%
    mutate(nyear = length(unique(Year)))%>%
    filter(nyear>=10,
           #max(value)-min(value)>2
    )%>%
    dplyr::summarize(monthly_correlation = ppcor::pcor.test(!!variable, !!value, 
                                                            Year, method = "spearman")$estimate,
    )
  
  labs = c("N:     Sep\nS:     Mar","Oct\nApr","Nov\nMay","Dec\nJun","Jan\nJul",
           "Feb\nAug","Mar\nSep","Apr\nOct","May\nNov","Jun\nDec","Jul\nJan","Aug\nFeb")
  
  anova_res <- aov(monthly_correlation~as.factor(mon),
                   data=many_lake_stat)
  tukey <- TukeyHSD(anova_res)
  cld <- multcompView::multcompLetters4(anova_res,tukey)$`as.factor(mon)`$Letters
  cld_df <- data.frame(cld = cld, mon = names(cld))%>%
    mutate(mon = factor(as.numeric(mon), levels = c(9:12,1:8), labels = labs))
  wilcox <- many_lake_stat%>%
    group_by(mon)%>%
    mutate(p = wilcox.test(monthly_correlation)$p.value,
           n = n(),
           mon = factor(mon, levels = c(9:12,1:8),labels = labs))
  fig <- wilcox %>%
    ggplot(aes(x=mon, y = monthly_correlation))+
    geom_boxplot(aes(color=p<alpha))+
    geom_text(aes(x = mon, label = cld, y = 1.08),
              data = cld_df)+
    #geom_text(aes(label = paste0("n = ",n), y = max(do_temp)))+ #n is the same across all
    geom_hline(yintercept=0)+
    ylim(-1,1.08)+
    xlab("Air temperature month")+
    ylab(paste0("Correlation with\n",name,"\n(n = ",unique(wilcox$n),")"))+
    theme_bw()+
    scale_color_manual(values = c("grey50","#FB8B24"))+
    theme(axis.text.x = element_text(hjust = ifelse(
      levels(wilcox$mon) == "N:     Sep\nS:     Mar", .8, .5)))+
    labs(color = paste0("p < ",alpha))
  return(fig)
}

daily_cors <- function(lake, data, variable, value) {
  result <- data %>%
    filter(LakeID == lake) %>%
    group_by(LakeID, doy) %>%
    summarize(monthly_correlation = ppcor::pcor.test(!!variable, !!value, 
                                                      Year, method = "spearman")$estimate)
  
  return(result)
}

monthly_correlations_doy <- function(wi_lakes_all_data, variable, value, name, alpha = 0.001) {
  #Parse variable name
  variable <- sym(variable)
  value <- sym(value)
  
  many_lake_stat1 <- wi_lakes_all_data %>%
    dplyr::select(LakeID, doy, Year, !!variable, !!value) %>%
    filter(!is.na(!!variable), !is.na(!!value), doy < yday("2022-08-31")) %>%
    group_by(LakeID, doy) %>%
    mutate(nyear = length(unique(Year))) %>%
    filter(nyear>=10,
           #max(value)-min(value)>2
    ) %>%
    dplyr::select(LakeID, doy, Year, !!variable, !!value)
  
  many_lake_stat <- purrr::map(unique(many_lake_stat1$LakeID), 
             daily_cors, data = many_lake_stat1, variable = variable, value = value) %>%
    list_rbind()
  
  wilcox <- many_lake_stat%>%
    group_by(doy)%>%
    mutate(p = wilcox.test(monthly_correlation)$p.value,
           n = n())
  return(wilcox)
}

#' Monthly correlations poster/talk
#'
#' @param wi_lakes_all_data data frame
#' @param variable variable to plot
#' @param value value to correlate with (e.g., air temperature)
#' @param name name for y axis
#' @param alpha alpha value for significance
#'
#' @return
#' @export
#'
#' @examples
monthly_correlations_talk <- function(wi_lakes_all_data, 
                                      variable, 
                                      value, 
                                      name, 
                                      alpha = 0.001,
                                      highlight = NULL) {
  #Parse variable name
  variable <- sym(variable)
  value <- sym(value)
  
  many_lake_stat <- wi_lakes_all_data %>%
    filter(!is.na(!!variable), !is.na(mon), !is.na(!!value))%>%
    group_by(LakeID, mon)%>%
    mutate(nyear = length(unique(Year)))%>%
    filter(nyear>=10,
           #max(value)-min(value)>2
    )%>%
    dplyr::summarize(monthly_correlation = ppcor::pcor.test(!!variable, !!value, 
                                                            Year, method = "spearman")$estimate,
    )%>%
    filter(mon < 9)
  
  labs = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
  
  anova_res <- aov(monthly_correlation~as.factor(mon),
                   data=many_lake_stat)
  tukey <- TukeyHSD(anova_res)
  cld <- multcompView::multcompLetters4(anova_res,tukey)$`as.factor(mon)`$Letters
  cld_df <- data.frame(cld = cld, mon = names(cld))%>%
    mutate(mon = factor(as.numeric(mon), levels = c(1:8), labels = labs))
  wilcox <- many_lake_stat%>%
    group_by(mon)%>%
    mutate(p = wilcox.test(monthly_correlation)$p.value,
           n = n(),
           mon = factor(mon, levels = c(1:8),labels = labs))
  fig_start <- wilcox %>%
    ggplot()+
    geom_violin(aes(x = mon, y = monthly_correlation, color=p<alpha), 
                         draw_quantiles = 0.5)
  if(!is.null(highlight)){
    fig_start <- fig_start + 
      geom_rect(aes(xmin = highlight[[1]], xmax = highlight[[2]], ymin = -Inf, ymax = Inf), 
                fill = "grey80", alpha = 0.5, data = data.frame()) }
  fig <- fig_start + 
    geom_violin(aes(x = mon, y = monthly_correlation, color=p<alpha), 
                         draw_quantiles = 0.5)+
    geom_text(aes(x = mon, label = cld, y = 1.08),
              data = cld_df)+
    #geom_text(aes(label = paste0("n = ",n), y = max(do_temp)))+ #n is the same across all
    geom_hline(yintercept=0)+
    ylim(-1,1.08)+
    xlab("Air temperature month")+
    ylab("Correlation")+
    ggtitle(paste0(name,"(n = ",unique(wilcox$n),")"))+
    theme_bw()+
    scale_color_manual(values = c("grey40","#0FA9E6"), breaks = c(F, T))+
    theme(axis.text.x = element_text(hjust = .5))+
    labs(color = paste0("p < ",alpha))
  return(fig)
}

#' Monthly correlations poster/talk only August data
#'
#' @param wi_lakes_all_data data frame
#' @param variable variable to plot
#' @param value value to correlate with (e.g., air temperature)
#' @param name name for y axis
#' @param alpha alpha value for significance
#'
#' @return
#' @export
#'
#' @examples
monthly_correlations_talk_aug <- function(wi_lakes_all_data, 
                                          variable, 
                                          value, 
                                          name, 
                                          alpha = 0.001) {
  #Parse variable name
  variable <- sym(variable)
  value <- sym(value)
  
  many_lake_stat <- wi_lakes_all_data %>%
    filter(!is.na(!!variable), !is.na(mon), !is.na(!!value))%>%
    group_by(LakeID, mon)%>%
    mutate(nyear = length(unique(Year)))%>%
    filter(nyear>=10,
           #max(value)-min(value)>2
    )%>%
    dplyr::summarize(monthly_correlation = ppcor::pcor.test(!!variable, !!value, 
                                                            Year, method = "spearman")$estimate,
    )%>%
    filter(mon < 9)
  
  labs = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
  
  anova_res <- aov(monthly_correlation~as.factor(mon),
                   data=many_lake_stat)
  tukey <- TukeyHSD(anova_res)
  cld <- multcompView::multcompLetters4(anova_res,tukey)$`as.factor(mon)`$Letters
  cld_df <- data.frame(cld = cld, mon = names(cld))%>%
    mutate(mon = factor(as.numeric(mon), levels = c(1:8), labels = labs))
  wilcox <- many_lake_stat%>%
    group_by(mon)%>%
    mutate(p = wilcox.test(monthly_correlation)$p.value,
           n = n(),
           mon = factor(mon, levels = c(1:8),labels = labs))
  fig <- wilcox %>%
    mutate(sig = p<alpha, 
           sig = factor(sig, levels = c(F,T))) %>%
    ggplot(aes(x=mon, y = monthly_correlation))+
    geom_boxplot(aes(color=sig), data = . %>% filter(mon == "Aug"))+
    geom_text(aes(x = mon, label = cld, y = 1.08),
              data = cld_df %>% filter(mon == "Aug"), alpha = 0)+
    #geom_text(aes(label = paste0("n = ",n), y = max(do_temp)))+ #n is the same across all
    geom_hline(yintercept=0)+
    ylim(-1,1.08)+
    xlab("Air temperature month")+
    ylab(paste0("Correlation with\n",name,"\n(n = ",unique(wilcox$n),")"))+
    theme_bw()+
    scale_color_manual(values = c("grey50","#FB8B24"), 
                       breaks = c(F, T), 
                       labels = c("FALSE", "TRUE"),
                       drop = F)+
    scale_x_discrete(drop = F)+
    theme(axis.text.x = element_text(hjust = .5))+
    labs(color = paste0("p < ",alpha))
  return(fig)
}

#' Spring/summer correlations for poster/talk
#'
#' @param wi_lakes_all_data data frame
#' @param variable variable to plot
#' @param value value to correlate with (e.g., air temperature)
#' @param name name for y axis
#' @param alpha alpha value for significance
#'
#' @return
#' @export
#'
#' @examples
monthly_correlations_spring_summer <- function(wi_lakes_all_data, 
                                          variable, 
                                          value, 
                                          name, 
                                          alpha = 0.001) {
  #Parse variable name
  variable <- sym(variable)
  value <- sym(value)
  
  many_lake_stat <- wi_lakes_all_data %>%
    filter(!is.na(!!variable), !is.na(mon), !is.na(!!value))%>%
    group_by(LakeID, mon)%>%
    mutate(nyear = length(unique(Year)))%>%
    filter(nyear>=10,
           #max(value)-min(value)>2
    )%>%
    dplyr::summarize(monthly_correlation = ppcor::pcor.test(!!variable, !!value, 
                                                            Year, method = "spearman")$estimate,
    )%>%
    filter(mon %in% c(4, 8))
  
  labs = c("Spring air\ntemperature", "Summer air\ntemperature")
  
  wilcox <- many_lake_stat%>%
    group_by(mon)%>%
    mutate(p = wilcox.test(monthly_correlation)$p.value,
           n = n(),
           mon = factor(mon, levels = c(4, 8), labels = labs))
  fig <- wilcox %>%
    mutate(sig = p<alpha, 
           sig = factor(sig, levels = c(F,T))) %>%
    ggplot(aes(x=mon, y = monthly_correlation))+
    geom_boxplot(aes(color = sig))+
    #geom_text(aes(label = paste0("n = ",n), y = max(do_temp)))+ #n is the same across all
    geom_hline(yintercept=0)+
    scale_color_manual(values = c("grey50","#FB8B24"), 
                       breaks = c(F, T), 
                       labels = c("FALSE", "TRUE"),
                       drop = F)+
    ylim(-1,1)+
    ylab(paste0("Correlation\n(n = ",unique(wilcox$n),")"))+
    theme_bw()+
    #scale_x_discrete(drop = F)+
    theme(axis.text.x = element_text(hjust = .5), 
          axis.title.x = element_blank(),
          legend.position = "right")+
    labs(color = paste0("p < ",alpha))
  
  return(fig)
}
