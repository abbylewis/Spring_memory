library(hydroGOF)

run_rf_analysis <- function(potential_drivers, mem_rf, 
                            color_limits = c(-0.45, 0.45),
                            color_vals = c("#c16586", "#735690", "#25469a"),
                            ylab_text = "Partial dependence of CSEM"){
  responses <- c("epi_temp")
  mem_rf_df_surf_temp <- mem_rf %>%
    select(all_of(c(responses, potential_drivers))) %>%
    na.omit() %>%
    mutate(target = "Surface-water\ntemperature")
  rf1 <- randomForest(
    as.formula(paste(responses, "~", paste(potential_drivers, collapse = " + "))), 
    data = mem_rf_df_surf_temp, 
    ntree = 1000, 
    importance = T)
  rf1_r2 = rf1$rsq[length(rf1$rsq)]
  oob_pred <- rf1$predicted
  kge_epi_temp <- KGE(oob_pred, mem_rf_df_surf_temp$epi_temp)
  ImpData_rf1 <- as.data.frame(importance(rf1)) %>%
    mutate(Var.Names = row.names(.)) %>%
    mutate(target = "Surface-water\ntemperature")
  partials_surf_temp <- pdp::partial(rf1, pred.var = potential_drivers[1], 
                                     train = as.data.frame(mem_rf_df_surf_temp)) %>%
    pivot_longer(-yhat, names_to = "var", values_to = "x") %>%
    rename(y = yhat)
  for(i in 2:length(potential_drivers)){
    new <- pdp::partial(rf1, pred.var = potential_drivers[i], 
                        train = as.data.frame(mem_rf_df_surf_temp)) %>%
      pivot_longer(-yhat, names_to = "var", values_to = "x") %>%
      rename(y = yhat)
    partials_surf_temp = rbind(partials_surf_temp, new)
  }
  
  responses <- c("hypo_temp")
  mem_rf_df_hypo_temp <- mem_rf %>%
    select(all_of(c(responses, potential_drivers))) %>%
    na.omit() %>%
    mutate(target = "Bottom-water\ntemperature")
  rf2 <- randomForest(
    as.formula(paste(responses, "~", paste(potential_drivers, collapse = " + "))), 
    data = mem_rf_df_hypo_temp, 
    ntree = 1000, 
    importance = T)
  rf2_r2 = rf2$rsq[length(rf2$rsq)]
  oob_pred <- rf2$predicted
  kge_hypo_temp <- KGE(oob_pred, mem_rf_df_hypo_temp$hypo_temp)
  ImpData_rf2 <- as.data.frame(importance(rf2)) %>%
    mutate(Var.Names = row.names(.)) %>%
    mutate(target = "Bottom-water\ntemperature")
  partials_hypo_temp <- pdp::partial(rf2, pred.var = potential_drivers[1], 
                                     train = as.data.frame(mem_rf_df_hypo_temp)) %>%
    pivot_longer(-yhat, names_to = "var", values_to = "x") %>%
    rename(y = yhat)
  for(i in 2:length(potential_drivers)){
    new <- pdp::partial(rf2, pred.var = potential_drivers[i], 
                        train = as.data.frame(mem_rf_df_hypo_temp)) %>%
      pivot_longer(-yhat, names_to = "var", values_to = "x") %>%
      rename(y = yhat)
    partials_hypo_temp = rbind(partials_hypo_temp, new)
  }
  
  responses <- c("hypo_do")
  mem_rf_df_hypo_do <- mem_rf %>%
    select(all_of(c(responses, potential_drivers, "DO_mgL_BOT"))) %>%
    na.omit() %>%
    mutate(target = "Bottom-water\ndissolved oxygen") %>%
    filter(DO_mgL_BOT >= 1)
  rf3 <- randomForest(
    as.formula(paste(responses, "~", paste(potential_drivers, collapse = " + "))), 
    data = mem_rf_df_hypo_do, 
    ntree = 1000, 
    importance = T)
  rf3_r2 = rf3$rsq[length(rf3$rsq)]
  oob_pred <- rf3$predicted
  kge_hypo_do <- KGE(oob_pred, mem_rf_df_hypo_do$hypo_do)
  ImpData_rf3 <- as.data.frame(importance(rf3)) %>%
    mutate(Var.Names = row.names(.)) %>%
    mutate(target = "Bottom-water\ndissolved oxygen")
  partials_hypo_do <- pdp::partial(rf3, pred.var = potential_drivers[1], 
                                   train = as.data.frame(mem_rf_df_hypo_do)) %>%
    pivot_longer(-yhat, names_to = "var", values_to = "x") %>%
    rename(y = yhat)
  for(i in 2:length(potential_drivers)){
    new <- pdp::partial(rf3, pred.var = potential_drivers[i], 
                        train = as.data.frame(mem_rf_df_hypo_do)) %>%
      pivot_longer(-yhat, names_to = "var", values_to = "x") %>%
      rename(y = yhat)
    partials_hypo_do = rbind(partials_hypo_do, new)
  }
  
  responses <- c("vhod")
  mem_rf_df_vhod <- mem_rf %>%
    select(all_of(c(responses, potential_drivers))) %>%
    na.omit() %>%
    mutate(target = "Bottom-water\noxygen demand")
  rf4 <- randomForest(
    as.formula(paste(responses, "~", paste(potential_drivers, collapse = " + "))), 
    data = mem_rf_df_vhod, 
    ntree = 1000, 
    importance = T)
  rf4_r2 = rf4$rsq[length(rf4$rsq)]
  oob_pred <- rf4$predicted
  kge_vhod <- KGE(oob_pred, mem_rf_df_vhod$vhod)
  ImpData_rf4 <- as.data.frame(importance(rf4)) %>%
    mutate(Var.Names = row.names(.)) %>%
    mutate(target = "Bottom-water\noxygen demand")
  partials_vhod <- pdp::partial(rf4, pred.var = potential_drivers[1], 
                                train = as.data.frame(mem_rf_df_vhod)) %>%
    pivot_longer(-yhat, names_to = "var", values_to = "x") %>%
    rename(y = yhat)
  for(i in 2:length(potential_drivers)){
    new <- pdp::partial(rf4, pred.var = potential_drivers[i], 
                        train = as.data.frame(mem_rf_df_hypo_do)) %>%
      pivot_longer(-yhat, names_to = "var", values_to = "x") %>%
      rename(y = yhat)
    partials_vhod = rbind(partials_vhod, new)
  }
  
  rug_data <- mem_rf_df_surf_temp %>%
    full_join(mem_rf_df_hypo_temp) %>%
    full_join(mem_rf_df_vhod) %>%
    full_join(mem_rf_df_hypo_do) %>%
    pivot_longer(cols = all_of(potential_drivers),
                 names_to = "var", values_to = "x") %>%
    mutate(var = recode(var, 
                        "log_SA" = "Log surface\narea (km^2)",
                        "log_max_depth" = "Log maximum\ndepth (m)",
                        "buoyancy_freq" = "Buoyancy\nfrequency (1/s)",
                        "abs_lat" = "Absolute latitude\n(DD)",
                        "log_DOC" = "Log DOC\n(mg/L)",
                        "log_chla" = "Log chlorophyll-a\n(ug/L)",
                        "log_TP" = "Log TP\n(µg/L)")) %>%
    mutate(target = factor(target, 
                           levels = c("Surface-water\ntemperature",
                                      "Bottom-water\ntemperature", 
                                      "Bottom-water\noxygen demand",
                                      "Bottom-water\ndissolved oxygen"))) %>%
    filter(!is.na(x))
  
  r2s <- data.frame(target = c("Surface-water\ntemperature",
                               "Bottom-water\ntemperature", 
                               "Bottom-water\noxygen demand",
                               "Bottom-water\ndissolved oxygen"),
                    r2 = c(rf1_r2, rf2_r2, rf4_r2, rf3_r2),
                    kges = c(kge_epi_temp, kge_hypo_temp, kge_vhod, kge_hypo_do),
                    ns = c(nrow(mem_rf_df_surf_temp),
                           nrow(mem_rf_df_hypo_temp),
                           nrow(mem_rf_df_vhod),
                           nrow(mem_rf_df_hypo_do))) %>%
    mutate(lab = paste0("paste('n = ", ns, " lakes;'~R^2~'= ",round(r2, 2), 
                        "; KGE = ", round(kges, 2), "')"))
  
  ImpData <- ImpData_rf1 %>%
    bind_rows(ImpData_rf2) %>%
    bind_rows(ImpData_rf3) %>%
    bind_rows(ImpData_rf4) %>%
    left_join(r2s) %>%
    mutate(target = factor(target, levels = c("Surface-water\ntemperature",
                                              "Bottom-water\ntemperature", 
                                              "Bottom-water\noxygen demand",
                                              "Bottom-water\ndissolved oxygen")))
  
  imp_mem <- ImpData %>%
    mutate(Var.Names = recode(Var.Names, "log_SA" = "Log surface\narea",
                              "log_max_depth" = "Maximum\ndepth",
                              "buoyancy_freq" = "Buoyancy\nfrequency",
                              "log_DOC" = "DOC",
                              "abs_lat" = "Absolute\nlatitude",
                              "log_chla" = "Log chlorophyll-a",
                              "log_TP" = "Log TP"),
           Var.Names = factor(Var.Names),
           Var.Names = fct_relevel(Var.Names, "Buoyancy\nfrequency", 
                                   "Maximum\ndepth",
                                   "Log surface\narea", 
                                   "Absolute\nlatitude"),
           xlab = "") %>%
    ggplot(aes(y = `%IncMSE`, x = Var.Names)) +
    geom_col() +
    geom_text(aes(x = 0.7, y = 83, label = lab), 
              hjust = 0, size = 3, parse = T,
              data = . %>% select(target, lab) %>% distinct())+
    coord_cartesian(clip = "off") +
    facet_grid(target~xlab, switch = "both") + 
    labs(y = "Variable importance (% increase in MSE)") + 
    ylim(c(0,90))+
    theme_bw() +
    theme(axis.title.x = element_blank(),
          strip.placement = "outside",   # format to look like title
          strip.background = element_blank(),
          strip.clip = "off",
          panel.spacing.y = unit(0.8, "lines"),
          axis.title.y = element_text(size = 9))
  
  M_part <- partials_surf_temp %>%
    mutate(target = "Surface-water\ntemperature") %>%
    bind_rows(partials_hypo_temp %>%
                mutate(target = "Bottom-water\ntemperature")) %>%
    bind_rows(partials_hypo_do%>%
                mutate(target = "Bottom-water\ndissolved oxygen")) %>%
    bind_rows(partials_vhod%>%
                mutate(target = "Bottom-water\noxygen demand"))  %>%
    mutate(Type = "M") %>%
    mutate(var = recode(var, 
                        "log_SA" = "Log surface\narea (km^2)",
                        "log_max_depth" = "Log maximum\ndepth (m)",
                        "buoyancy_freq" = "Buoyancy\nfrequency (1/s)",
                        "abs_lat" = "Absolute latitude\n(DD)",
                        "log_DOC" = "Log DOC\n(mg/L)",
                        "log_chla" = "Log chlorophyll-a\n(ug/L)",
                        "log_TP" = "Log TP\n(µg/L)"))
  
  part <- M_part %>%
    mutate(target = factor(target, 
                           levels = c("Surface-water\ntemperature",
                                      "Bottom-water\ntemperature", 
                                      "Bottom-water\noxygen demand",
                                      "Bottom-water\ndissolved oxygen"))) %>%
    group_by(Type, var, target) %>%
    arrange(x, .by_group = TRUE) %>%
    reframe(x_interp = approx(x = x, y = y, 
                              xout = seq(min(x), max(x), 
                                         length.out = 10000))$x,
            y_interp = approx(x = x, y = y, 
                              xout = seq(min(x), max(x), 
                                         length.out = 10000))$y) %>%
    mutate(var = factor(var),
           var = fct_relevel(var, "Buoyancy\nfrequency (1/s)", 
                             "Log surface\narea (km^2)", 
                             "Absolute latitude\n(DD)")) %>%
    ggplot(aes(x = x_interp)) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
    geom_point(aes(y = y_interp, color = y_interp), size = 0.1) +
    geom_rug(aes(x = x), sides = "b", 
             data = rug_data %>%
               mutate(var = factor(var),
                      var = fct_relevel(var, "Buoyancy\nfrequency (1/s)", 
                                        "Log surface\narea (km^2)", 
                                        "Absolute latitude\n(DD)")), 
             alpha = 0.4, outside = TRUE, length = unit(0.08, "npc")) +
    coord_cartesian(clip = "off") +
    facet_grid(target~var, scales = "free_x", switch = "both") +
    labs(x=NULL) +
    theme_bw() +
    scale_color_gradientn(colors = color_vals, 
                          limits = color_limits) +
    theme(
      strip.placement = "outside",   # format to look like title
      strip.background = element_blank(),
      #strip.text.y = element_blank(),
      strip.clip = "off",
      panel.spacing.y = unit(0.8, "lines"),
      axis.text.x = element_text(vjust=-0.5),
      axis.title.y = element_text(size = 9),
      legend.position = "none"
    ) +
    ylab(ylab_text)
  
  return(ggarrange(imp_mem, part, widths = c(0.8, 1)))
}
