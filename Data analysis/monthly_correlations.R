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
    xlab("TP input month")+
    ylab(paste0("Correlation with\n",name,"\n(n = ",unique(wilcox$n),")"))+
    theme_bw()+
    scale_color_manual(values = c("grey50","#FB8B24"))+
    theme(axis.text.x = element_text(hjust = ifelse(
      levels(wilcox$mon) == "N:     Sep\nS:     Mar", .8, .5)))+
    labs(color = paste0("p < ",alpha))
  return(fig)
}
