group_fun <- function(sig) {
  group <- rep(1, length(sig))
  for (i in 2:length(sig)) {
    if(sig[i] == sig[i - 1]) {
      group[i] <- group[i - 1]
    } else {
      group[i] <- group[i - 1] + 1
    }
  }
  return(group)
}

plot_single_var <- function(var_name){
  all_plot <- all %>%
    filter(var == var_name) %>%
    group_by(var) %>%
    mutate(n = max(n), #Some lakes are missing from January because our air temp only goes back to 1980
           var = paste0(var, " (n = ",n,")")) %>%
    group_by(var, doy, sig) %>%
    summarise(mean = mean(monthly_correlation, na.rm = T),
              sd = sd(monthly_correlation, na.rm = T)) 
  
  all_plot$group <- 1
  for(i in 1:length(unique(all_plot$var))){
    all_plot$group[all_plot$var == unique(all_plot$var)[i]] <- group_fun(all_plot$sig[all_plot$var == unique(all_plot$var)[i]])
  }
  
  empty_df <- data.frame(var = unique(all_plot$var)) %>%
    filter(var == var_name) 
  
  all_plot1 <- all_plot %>%
    mutate(DateTime = as.Date("2022-01-01") + days(doy))
  
  all_plot2 <- all_plot %>%
    ungroup() %>%
    mutate(DateTime = as.Date("2022-01-01") + days(doy) + hours(23) + minutes(59),
           mean = ifelse(!is.na(lead(var)) & lead(var) == var, lead(mean), mean), #to get smooth lines
           sd = ifelse(lead(var) == var, lead(sd), sd))
  
  colored_sd <- all_plot1 %>%
    full_join(all_plot2) %>%
    #filter(!var == "Summer epi. DO (n = 397)") %>%
    #mutate(var = factor(var, levels = c("Summer epi. temperature (n = 413)",
    #                                    "Summer hypo. temperature (n = 368)",
    #                                    "Summer VHOD (n = 194)",
    #                                    "Summer hypo. DO (n = 286)"))) %>%
    ggplot() + 
    #geom_rect(aes(xmin = as_datetime("2022-03-01"), 
    #              xmax = as_datetime("2022-05-31"), 
    #              ymin = -Inf, ymax = Inf), 
    #              fill = "grey80", alpha = 0.3, 
    #          data = empty_df) +
    #geom_rect(aes(xmin = as_datetime("2022-03-01") + days(30), 
    #              xmax = as_datetime("2022-05-31") + days(30), 
    #              ymin = -Inf, ymax = Inf), 
    #              fill = "grey80", alpha = 0.3, 
    #          data = empty_df) +
    #geom_rect(aes(xmin = as_datetime("2022-07-01"), xmax = as_datetime("2022-08-31"), 
  #              ymin = -Inf, ymax = Inf), 
  #              fill = "grey80", alpha = 0.3, 
  #          data = empty_df) +
  #geom_rect(aes(xmin = as_datetime("2022-07-01") + days(30), 
  #              xmax = as_datetime("2022-08-31") + days(30), 
  #              ymin = -Inf, ymax = Inf), 
  #              fill = "grey80", alpha = 0.3, 
  #          data = empty_df) +
  geom_hline(yintercept=0)+
    geom_vline(xintercept = as_datetime("2022-04-21"), color = "grey50", lty = "dashed")+
    geom_ribbon(aes(x = DateTime, 
                    ymin = mean - sd, ymax = mean + sd,
                    fill = sig, group = group),
                alpha = 0.5) +
    geom_line(aes(x = DateTime, y = mean)) +
    #geom_text(aes(x = as_datetime("2022-03-15") , y = 0.8, label = paste0("Spring")), 
    #          size = 3, color = "grey30",
    #          data = empty_df) +
    #geom_text(aes(x = as_datetime("2022-08-01") , y = 0.8, label = paste0("Summer")), 
    #          size = 3, color = "grey30",
    #          data = empty_df) +
    xlab("Air temperature date \n(right-aligned 30-day rolling mean)")+
    ylab("Correlation")+
    theme_bw()+
    scale_fill_manual(values = c("grey40","#0FA9E6"))+
    labs(fill = paste0("p < ",alpha)) +
    facet_wrap(~var, ncol = 2) 
  return(colored_sd)
}
