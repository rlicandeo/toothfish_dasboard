
## Programmer Roberto Licandeo
## robertolicandeo@gmail.com
## 28 Dec 2024
## updated 12 Nov 2025

kobeplotSS3 <- function(model_SS_output,ylim=2,xlim=4, maintxt="")
{
  
  DerQuants = model_SS_output$derived_quants
  indxForeCatch <- grep("ForeCatch_", DerQuants$Label)
  ForeCatch <- DerQuants[indxForeCatch, 1:3]
  last_year <- head(ForeCatch, n = 1)
  last_year_label <- last_year$Label 
  last_yearData <- as.numeric(sub("ForeCatch_", "", last_year_label)) -1
  Kobe_ts = model_SS_output$Kobe
  init_yr=Kobe_ts[1,"Yr"]
  
  Kobe_Data <- Kobe_ts[Kobe_ts$Yr %in% init_yr:last_yearData,1:3]
  
  mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
  n=dim(Kobe_Data)[1]
  yr=Kobe_Data[,"Yr"]
  par(mfcol=c(1,1),xaxs="i",yaxs="i")
  bstat<-Kobe_Data[,"B.Bmsy"]
  ustat<-Kobe_Data[,"F.Fmsy"]
  plot(bstat,ustat,type="o",
       xlab="SBt/SBmsy",
       ylab="Ft/Fmsy",
       ylim=c(0,ylim),xlim=c(0,xlim),pch="x", main=maintxt)
  rect(0,1,1,ylim,col="salmon")
  rect(0,0,1,1,col="lightyellow")
  rect(1,1,xlim,ylim,col="lightyellow")
  rect(1,0,xlim,1,col="lightgreen")
  
  lines(bstat,ustat,type="o")
  points(bstat[c(1,n)],ustat[c(1,n)], pch = 19, cex = 2, col = mycol)
  text(bstat[c(1,n)],ustat[c(1,n)], paste(yr[c(1,n)]), col="blue" )
  text(0.5,1.5,"Overfished\n&\nOverfishing",font=2)
  text(0.5,0.5,"Overfished",font=2)
  text(1.5,1.5,"Overfishing",font=2)
}


getKobeTs=function(model_SS_output) {
  DerQuants = model_SS_output$derived_quants
  indxForeCatch <- grep("ForeCatch_", DerQuants$Label)
  ForeCatch <- DerQuants[indxForeCatch, 1:3]
  last_year <- head(ForeCatch, n = 1)
  last_year_label <- last_year$Label 
  last_yearData <- as.numeric(sub("ForeCatch_", "", last_year_label)) -1
  Kobe_ts = model_SS_output$Kobe
  init_yr=Kobe_ts[1,"Yr"]
  Kobe_Data <- Kobe_ts[Kobe_ts$Yr %in% init_yr:last_yearData,1:3]
  row.names(Kobe_Data) <- NULL
  Kobe_Data
}


# calculate_prob_catch <- function(model_SS_output) {
# 
#   DerQuants = model_SS_output$derived_quants
#   indxForeCatch <- grep("ForeCatch_", DerQuants$Label)
#   ForeCatch <- DerQuants[indxForeCatch, 1:3]
#   init_year <- head(ForeCatch, n = 1)
#   last_year <- tail(ForeCatch, n = 1)
#   FistYrPrj <- as.numeric(sub("ForeCatch_", "", init_year$Label))
#   LastYrPrj <- as.numeric(sub("ForeCatch_", "", last_year$Label))
# 
#   ssb_Prj <- DerQuants[DerQuants$Label %in% paste0("SSB_", FistYrPrj:LastYrPrj),1:3]
#   catch_Prj = DerQuants[DerQuants$Label %in% paste0("ForeCatch_", FistYrPrj:LastYrPrj),1:3]
# 
#   # Calculate Bmsy_mean
#   SSB_MSY = DerQuants[DerQuants$Label %in% "SSB_MSY",1:3]
#   SSBmsy_mean <- SSB_MSY$Value
# 
#   # Set up scenarios and results list
#   ## RL: more scenarios could be added here (only 1 for now)
#   scenarios <- c("Fmsy")
#   prob_results <- vector("list", length(scenarios))
#   names(prob_results) <- scenarios
# 
#   for(i in 1:length(scenarios)) {
# 
#     ## Calculate probability for SSB
#     ssb_probs <- (1 - pnorm(SSBmsy_mean, ssb_Prj[,"Value"], ssb_Prj[,"StdDev"])) * 100
# 
#     # Combine results
#     prob_results[[i]] <- cbind(
#       Year = FistYrPrj:LastYrPrj ,
#       SSB_prob = round(ssb_probs, 2),
#       Catch = round(catch_Prj[,"Value"], 2),
#       SSB = round(ssb_Prj[,"Value"], 2),
#       SD = round(ssb_Prj[,"Value"], 2)
#     )
#   }
# 
# 
#   return(prob_results)
# }



plot_prSSBAboveBmsy_catch <- function(results, maintex="",
                                      scenarios_to_plot = c("0.75x", "Fmsy", "0x"),
                                      end_year = 2041) {
  ## RL: it depends from outputs from calculate_prob_catch()
  available_scenarios <- names(results)
  valid_scenarios <- scenarios_to_plot[scenarios_to_plot %in% available_scenarios]

  if(length(valid_scenarios) == 0) {
    stop("None of the requested scenarios found in results")
  }

  plot_data <- do.call(rbind, lapply(valid_scenarios, function(scenario) {
    data <- data.frame(
      Year = results[[scenario]][,"Year"],
      SSB_prob = results[[scenario]][,"SSB_prob"],
      Catch = results[[scenario]][,"Catch"],
      Scenario = scenario
    )
    # Filter data up to end_year
    data[data$Year <= end_year, ]
  }))

  max_catch <- max(plot_data$Catch)

  p <- ggplot(plot_data, aes(x = Year)) +
    geom_line(aes(y = SSB_prob, color = Scenario), linewidth = 1) +
    geom_point(aes(y = SSB_prob, color = Scenario), size = 2) +
    geom_line(aes(y = Catch * 100/max_catch, color = Scenario),
              linewidth = 1, linetype = "dashed") +
    scale_y_continuous(
      name = "SSB Probability (% above SBmsy)",
      breaks = seq(0, 100, by = 10),
      limits = c(0, NA),
      sec.axis = sec_axis(~ . * max_catch / 100,
                          name = "Catch (---)",
                          breaks = round(seq(0, max_catch, length.out = 11), 0))
    ) +
    scale_x_continuous(breaks = seq(min(plot_data$Year), max(plot_data$Year), by = 2)) +
    labs(
      x = "Year",
      title = paste0("SSB Probability and Catch by Scenario: ", maintex),
      color = "Scenario"
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 14),
      axis.title.y = element_text(size = 16, color = "black"),
      axis.title.y.right = element_text(size = 16, color = "black"),
      axis.title.x = element_text(size = 16),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.title = element_text(size = 18),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  return(p)
}


# plot_prSSBAboveBmsy_catch <- function(model_SS_output) {
#   DerQuants <- model_SS_output$derived_quants
#   indxForeCatch <- grep("ForeCatch_", DerQuants$Label)
#   ForeCatch <- DerQuants[indxForeCatch, 1:3]
#   
#   FistYrPrj <- as.numeric(sub("ForeCatch_", "", ForeCatch$Label[1]))
#   LastYrPrj <- as.numeric(sub("ForeCatch_", "", ForeCatch$Label[nrow(ForeCatch)]))
#   
#   ssb_Prj <- DerQuants[DerQuants$Label %in% paste0("SSB_", FistYrPrj:LastYrPrj), 1:3]
#   catch_Prj <- DerQuants[DerQuants$Label %in% paste0("ForeCatch_", FistYrPrj:LastYrPrj), 1:3]
#   
#   SSBmsy_mean <- DerQuants[DerQuants$Label == "SSB_MSY", "Value"]
#   ssb_probs <- (1 - pnorm(SSBmsy_mean, ssb_Prj$Value, ssb_Prj$StdDev)) * 100
#   
#   result <- data.frame(
#     Year = FistYrPrj:LastYrPrj,
#     SSB_prob = round(ssb_probs, 2),
#     Catch = round(catch_Prj$Value, 2),
#     SSB = round(ssb_Prj$Value, 2),
#     SD = round(ssb_Prj$StdDev, 2)
#   )
#   
#   return(result)
# }


# calculate_prob_catch <- function(model_SS_output) {
#   DerQuants <- model_SS_output$derived_quants
#   indxForeCatch <- grep("ForeCatch_", DerQuants$Label)
#   ForeCatch <- DerQuants[indxForeCatch, 1:3]
# 
#   FistYrPrj <- as.numeric(sub("ForeCatch_", "", ForeCatch$Label[1]))
#   LastYrPrj <- as.numeric(sub("ForeCatch_", "", ForeCatch$Label[nrow(ForeCatch)]))
# 
#   ssb_Prj <- DerQuants[DerQuants$Label %in% paste0("SSB_", FistYrPrj:LastYrPrj), 1:3]
#   catch_Prj <- DerQuants[DerQuants$Label %in% paste0("ForeCatch_", FistYrPrj:LastYrPrj), 1:3]
# 
#   SSBmsy_mean <- DerQuants[DerQuants$Label == "SSB_MSY", "Value"]
#   ssb_probs <- (1 - pnorm(SSBmsy_mean, ssb_Prj$Value, ssb_Prj$StdDev)) * 100
# 
#   result <- data.frame(
#     Year = FistYrPrj:LastYrPrj,
#     SSB_prob = round(ssb_probs, 2),
#     Catch = round(catch_Prj$Value, 2),
#     SSB = round(ssb_Prj$Value, 2),
#     SD = round(ssb_Prj$StdDev, 2)
#   )
# 
#   return(result)
# }


calculate_probSSB_catch <- function(model_SS_output) {
  DerQuants <- model_SS_output$derived_quants
  indxForeCatch <- grep("ForeCatch_", DerQuants$Label)
  ForeCatch <- DerQuants[indxForeCatch, 1:3]
  
  FistYrPrj <- as.numeric(sub("ForeCatch_", "", ForeCatch$Label[1]))
  LastYrPrj <- as.numeric(sub("ForeCatch_", "", ForeCatch$Label[nrow(ForeCatch)]))
  
  ssb_Prj <- DerQuants[DerQuants$Label %in% paste0("SSB_", FistYrPrj:LastYrPrj), 1:3]
  catch_Prj <- DerQuants[DerQuants$Label %in% paste0("ForeCatch_", FistYrPrj:LastYrPrj), 1:3]
  
  SSBmsy_mean <- DerQuants[DerQuants$Label == "SSB_MSY", "Value"]
  ssb_probs <- (1 - pnorm(SSBmsy_mean, ssb_Prj$Value, ssb_Prj$StdDev)) * 100
  
  result <- data.frame(
    Year = FistYrPrj:LastYrPrj,
    SSB_prob = round(ssb_probs, 2),
    Catch = round(catch_Prj$Value, 2),
    SSB = round(ssb_Prj$Value, 2),
    SD = round(ssb_Prj$StdDev, 2)
  )
  
  return(result)
}



getMSY_RP <- function(model_SS_output) {
  DerQuants <- model_SS_output$derived_quants
  Fmsy <- DerQuants[DerQuants$Label %in% "annF_MSY", 1:3]
  SSB_MSY <- DerQuants[DerQuants$Label %in% "SSB_MSY", 1:3]
  Catch_MSY <- DerQuants[DerQuants$Label %in% "Dead_Catch_MSY", 1:3]
  result_df <- data.frame(Fmsy = Fmsy$Value,
                          SSB_MSY = SSB_MSY$Value,
                          Catch_MSY = Catch_MSY$Value)
  
  rownames(result_df) <- NULL
  
  return(result_df)
}


# extract_data <- function(results, model_label) {
#   data.frame(
#     Year = results$Fmsy[, "Year"],
#     SSB_prob = results$Fmsy[, "SSB_prob"],
#     Catch = results$Fmsy[, "Catch"],
#     SSB = results$Fmsy[, "SSB"],
#     # SD = results$Fmsy[, "SD"],
#     xFmsy = rep(model_label, nrow(results$Fmsy))
#   )
# }



##- Function to read summaries, extract likelihoods, and create a summary table with limited decimals
create_summary_table <- function(model_paths, decimals = 2) {
  summaries <- lapply(model_paths, function(path) {
    SS_read_summary(file = paste0(path, "/ss_summary.sso"), verbose = FALSE)
  })
  
  likelihoods <- lapply(summaries, function(summary) summary$likelihoods)
  
  combined_lik <- do.call(cbind, likelihoods)
  
  colnames(combined_lik) <- paste0("model_", seq_along(model_paths))
  
  combined_lik <- round(combined_lik, decimals)
  
  kable_table <- knitr::kable(combined_lik, caption = "Summary Table") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)
  
  return(kable_table)
}




generate_forecast_blocks <- function(start_year, end_year, season = 1, values = c(0, 0, 0)) {
  years <- start_year:end_year
  output <- data.frame(
    Year = rep(years, each = length(values)),
    Season = season,
    Fleets = rep(seq_along(values), times = length(years)),
    Value = rep(values, times = length(years))
  )
  
  final_row <- data.frame(Year = -9999, Season = 1, Fleets = 1, Value = 0)
  output <- rbind(output, final_row)
  
  return(output)
}

