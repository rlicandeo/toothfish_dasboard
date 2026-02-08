

rm(list=ls()) 
mydir = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish"
# pak::pkg_install("r4ss/r4ss")
setwd(mydir)

library(r4ss) # use r4ss package on github
library(rlang)
library(ss3diags)


###################################
###### model 01 ##################


## copy_SS_inputs(dir.old = model_HCR0_path, dir.new = "model_01")
model_01_path <- file.path(mydir, "model_01")
run(dir = model_01_path, 
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")
## plotting 
model_01 <- SS_output(model_01_path, forecast = TRUE)
## generates a lot plots
SS_plots(model_01, forecastplot = TRUE, pdf = FALSE, png = TRUE)
## Residual Diagnostics
sspar(mfrow = c(3,2))
SSplotRunstest(model_01, add = TRUE)
SSplotRunstest(model_01, subplots = "len", add = TRUE)
## Create executive summary tables from an SS3
SSexecutivesummary(replist = model_01)


## model 02

##copy_SS_inputs(dir.old = model_01_path, dir.new = "model_02")
model_02_path <- file.path(mydir, "model_02")
run(dir = model_02_path,
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")
## plotting
model_02 <- SS_output(model_02_path, forecast = TRUE)
## generates a lot plots
SS_plots(model_02, forecastplot = TRUE, pdf = FALSE, png = TRUE)
## Residual Diagnostics
sspar(mfrow = c(3,2))
SSplotRunstest(model_02, add = TRUE)
SSplotRunstest(model_02, subplots = "len", add = TRUE)
## Create executive summary tables from an SS3
SSexecutivesummary(replist = model_02)



## model 03

##copy_SS_inputs(dir.old = model_02_path, dir.new = "model_03")
model_03_path <- file.path(mydir, "model_03")
run(dir = model_03_path,
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")
## plotting
model_03 <- SS_output(model_03_path, forecast = TRUE)
## generates a lot plots
SS_plots(model_03, forecastplot = TRUE, pdf = FALSE, png = TRUE)
## Residual Diagnostics
sspar(mfrow = c(3,2))
SSplotRunstest(model_03, add = TRUE)
SSplotRunstest(model_03, subplots = "len", add = TRUE)
## Create executive summary tables from an SS3
SSexecutivesummary(replist = model_03)


## model 04

##copy_SS_inputs(dir.old = model_03_path, dir.new = "model_04")
model_04_path <- file.path(mydir, "model_04")
run(dir = model_04_path,
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")
## plotting
model_04 <- SS_output(model_04_path, forecast = TRUE)
## generates a lot plots
SS_plots(model_04, forecastplot = TRUE, pdf = FALSE, png = TRUE)
## Residual Diagnostics
sspar(mfrow = c(3,2))
SSplotRunstest(model_04, add = TRUE)
SSplotRunstest(model_04, subplots = "len", add = TRUE)
## Create executive summary tables from an SS3
SSexecutivesummary(replist = model_04)


## model 05

##copy_SS_inputs(dir.old = model_01_path, dir.new = "model_05")
model_05_path <- file.path(mydir, "model_05")
run(dir = model_05_path,
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")
## plotting
model_05 <- SS_output(model_05_path, forecast = TRUE)
## generates a lot plots
SS_plots(model_05, forecastplot = TRUE, pdf = FALSE, png = TRUE)
## Residual Diagnostics
sspar(mfrow = c(3,2))
SSplotRunstest(model_05, add = TRUE)
SSplotRunstest(model_05, subplots = "len", add = TRUE)
## Create executive summary tables from an SS3
SSexecutivesummary(replist = model_05)



## model 06

### copy_SS_inputs(dir.old = model_01_path, dir.new = "model_06")
model_06_path <- file.path(mydir, "model_06")
run(dir = model_06_path,
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")
## plotting
model_06 <- SS_output(model_06_path, forecast = TRUE)
## generates a lot plots
SS_plots(model_06, forecastplot = TRUE, pdf = FALSE, png = TRUE)
## Residual Diagnostics
sspar(mfrow = c(3,2))
SSplotRunstest(model_06, add = TRUE)
SSplotRunstest(model_06, subplots = "len", add = TRUE)
## Create executive summary tables from an SS3
SSexecutivesummary(replist = model_06)



## model 07

## copy_SS_inputs(dir.old = model_06_path, dir.new = "model_07")
model_07_path <- file.path(mydir, "model_07")
run(dir = model_07_path,
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")
## plotting
model_07 <- SS_output(model_07_path, forecast = TRUE)
## generates a lot plots
SS_plots(model_07, forecastplot = TRUE, pdf = FALSE, png = TRUE)
## Residual Diagnostics
sspar(mfrow = c(3,2))
SSplotRunstest(model_07, add = TRUE)
SSplotRunstest(model_07, subplots = "len", add = TRUE)
## Create executive summary tables from an SS3
SSexecutivesummary(replist = model_07)


## model 08

## copy_SS_inputs(dir.old = model_07_path, dir.new = "model_08")
model_08_path <- file.path(mydir, "model_08")
run(dir = "model_08",
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")
## plotting
model_08 <- SS_output("model_08", forecast = TRUE)
## generates a lot plots
SS_plots(model_08, forecastplot = TRUE, pdf = FALSE, png = TRUE)
## Residual Diagnostics
sspar(mfrow = c(3,2))
SSplotRunstest(model_08, add = TRUE)
SSplotRunstest(model_08, subplots = "len", add = TRUE)
## Create executive summary tables from an SS3
SSexecutivesummary(replist = model_08)



## model 09
## copy_SS_inputs(dir.old = model_08_path, dir.new = "model_09")
model_09_path <- file.path(mydir, "model_09")
run(dir = "model_09",
    show_in_console = TRUE,
    skipfinished = F,
    # extras= "-stopph 0 -nohess",
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")
## plotting
model_09 <- SS_output("model_09", forecast = TRUE)
## generates a lot plots
SS_plots(model_09, forecastplot = TRUE, pdf = FALSE, png = TRUE)
## Residual Diagnostics
sspar(mfrow = c(3,2))
SSplotRunstest(model_09, add = TRUE)
SSplotRunstest(model_09, subplots = "len", add = TRUE)
## Create executive summary tables from an SS3
SSexecutivesummary(replist = model_09)



SSplotRunstest(model_01,add=T,legendcex=0.8,tickEndYr=F,xylabs=T,indexselect = c(1,3,4)[1])
legend("topleft",paste0(letters[c(1,3,5)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)
SSplotRunstest(model_01,subplots = "len",add=T,legendcex=0.8,tickEndYr=F,xylabs=T,indexselect = c(1,2,4)[i])
legend("topleft",paste0(letters[c(2,4,6)[i]],")"),y.intersp = -0.2,x.intersp = -0.7,bty="n",cex=l)


### model comparison 
models_summary <- SSsummarize(list(model_01, model_02, model_03, model_04,
                                   model_05, model_06, model_07, model_08,
                                   model_09))
SSplotComparisons(models_summary, subplots=c(2,4,6,8,10,12,13,15,16,20,21),
                  legendlabels = c("m01", "m02", "m03", "m04",
                                   "m05", "m06", "m07", "m08","m09"),
                  pdf=F, png=T, plotdir = model_01_path)









sspar(mfrow=c(1,2),plot.cex = 0.8)
SSplotJABBAres(model_03,subplots="cpue",add=T)
SSplotJABBAres(model_03,subplots="len",add=T, ylim = c(-0.2,0.2))


# Check starter file
starter = SSsettingsBratioF(model_03)
# Get uncertainty from MVLN for F/F_Btrg with original F setting F_abs

sspar(mfrow=c(1,1),plot.cex = 0.9)
#Fref = c("Ftrg") #to change from FMSY to Ftrg
mvn = SSdeltaMVLN(model_03, plot = F, Fref = c("MSY"))
kbproj = data.frame(mvn$kb)

#labels = mvn$labels
SSplotKobe(kbproj,fill=T,joint=F,posterior="kernel",ylab=expression(F/F[msy]),xlab=expression(SSB/SSB[msy]))

mvn$labels <- expression(SSB/SSB[trg], "F/F"[SB ~ 35], "SSB", "F", "Recruits", "Catch")
dev.print(jpeg,paste0("Plotdiags/Kobe_",Runs,".jpg"), width = 6.5, height = 6.5, res = 300, units = "in")

sspar(mfrow=c(2,3),plot.cex = 0.9)
SSplotEnsemble(mvn$kb,ylabs = mvn$labels,add=T)
dev.print(jpeg,paste0("Plotdiags/MLVN_trj_",Runs,".jpg"), width = 8, height = 6.5, res = 300, units = "in")


retroSummary <- r4ss::SSsummarize(retroModels)

sspar(mfrow=c(2,2),plot.cex = 0.9)
SSplotRetro(retroSummary,forecastrho = T,add=T,subplots="SSB",endyrvec = 2021:2016)
SSplotRetro(retroSummary,forecastrho = T,add=T,legend = F,xmin=2005,endyrvec = 2021:2016)
SSplotRetro(retroSummary, subplots = "F",add=T,legendloc = "left",legendcex = 0.8,endyrvec = 2020:2015)
SSplotRetro(retroSummary,subplots = "F", xmin=2005,forecastrho = T,add=T,legend = F,endyrvec = 2020:2015)


sspar(mfrow=c(1,1),plot.cex = 0.9)
SSplotHCxval(retroSummary,xmin=2006,add=T,legendcex = 0.6, Season=1)


hccomps = SSretroComps(retroModels)
sspar(mfrow=c(1,2),plot.cex = 0.7)
SSplotHCxval(hccomps,add=T,subplots = "len",legendloc="topleft",indexUncertainty = TRUE,legendcex = 0.6, Season=1)

sspar(mfrow=c(1,1),plot.cex = 0.7)
SSplotHCxval(retroSummary,add=T,subplots = "cpue",legendloc="topleft",indexUncertainty = TRUE,legendcex = 0.6)

SShcbias(retroSummary)

SSmase(hccomps,quants = "len")
SSmase(retroSummary,quants = "cpue", Season=c(1))

selShapes()


hc.age = lapply(hcs,function(x){
  hcl = ss3diags::SSretroComps(x)
  hcl
})

i = 1
sspar(mfrow=c(2,2),plot.cex = 0.65)
SSplotRetro(hc[[i]],add=T,legend=F,forecast=F,verbose = F)
SSplotRetro(hc[[i]],add=T,forecastrho = T,legend=F,verbose = F)
SSplotRetro(hc[[i]],subplots = "F",add=T,legend=F,forecast=F,
            verbose = F)
SSplotRetro(hc[[i]],subplots = "F",add=T,forecastrho = T,legend=F,
            verbose = F)
mtext(c("Retro","Forecast"),3,outer=T,line=-0.5,at=c(0.3,0.8),cex=0.8)









# 
# 
# 
# 
# 
# ### model comparisons 
# models_summary <- SSsummarize(list(model_01, model_02, model_03, model_04, model_05, model_06))
# SSplotComparisons(models_summary, subplots=c(2,4,6,8,10,12,13,15,16,20,21),
#                   legendlabels = c("m01", "m02", "m03", "m04", "m05", "m06"),
#                   pdf=F, png=T, plotdir = model_01_path)




### run model 01 with 0Fmsy;
## copy_SS_inputs(dir.old = model_01_path, dir.new = "model_01_0Fmsy") 
model_01_0Fmsy_path <- file.path(mydir, "model_01_0Fmsy")
## after copying we need to modify the forecast file
run(dir = model_01_0Fmsy_path,
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")
## plotting
model_01_0Fmsy <- SS_output(model_01_0Fmsy_path, forecast = TRUE)


### run model 01 with 0.75Fmsy;

nfleets =3
Fmult=0.75

## Read the file
lines <- readLines(paste0(model_01_path,"/","Forecast-report.sso"))
## Find the line index for Bmark_relF
relF_index <- grep("Bmark_relF\\(by_fleet_&seas\\)", lines)
## Extract the line that follows the label
relF_values <- strsplit(lines[relF_index + 1], " ")[[1]]
## Clean and convert to numeric
relF <- as.numeric(relF_values[relF_values != ""])[1:nfleets]
## get Fmsy
DerQuants <- model_01$derived_quants
Fmsy <- DerQuants[DerQuants$Label %in% "annF_MSY","Value"]

relF*Fmult*Fmsy
## 0.00000000 0.04109001 0.02364976

forecast_data <- generate_forecast_blocks(2024, 2028, season = 1, values=relF*Fmult*Fmsy)
print(forecast_data)
### you need to modify # N forecast years in forecast.ss for more years (no tested)

### copy model base only once (then comment the line to avoid overwrite the forecast modification. see below)
##copy_SS_inputs(dir.old = model_01_path, dir.new = "model_01_.75Fmsy")

model_01_.75Fmsy_path <- file.path(mydir, "model_01_.75Fmsy")

# Read the original forecast.ss file
lines = readLines(paste0(model_01_.75Fmsy_path,"/","forecast.ss"))
# Locate the line to replace (start of allocation group section)
start_line <- grep("#_if N allocation groups >0, list year, allocation fraction for each group", lines)
# Locate the line where forecast catch block begins (after "#_Yr Seas Fleet Catch(or_F)")
catch_block_start <- grep("#_Yr Seas Fleet Catch\\(or_F\\)", lines)
# Locate the line with -9999 that ends the catch block
catch_block_end <- grep("-9999\\s+1\\s+1\\s+0", lines)
# change value for basis for input Fcast catc
basis_line <- grep("^2\\s+# basis for input Fcast catch", lines)
lines[basis_line] <- "99 # basis for input Fcast catch: -1=read basis with each obs; 2=dead catch; 3=retained catch; 99=input Hrate(F)"

# Generate new forecast block
new_block <- forecast_data

# Format the block as character lines
formatted_block <- apply(new_block, 1, function(row) {
  sprintf("%5d %6d %6d %12.8f", row[1], row[2], row[3], row[4])
})

# Replace the old block with the new one
lines <- c(
  lines[1:(catch_block_start)],
  formatted_block,
  lines[(catch_block_end + 1):length(lines)]
)

# Write the updated file
## writeLines(lines,paste0(model_01_.75Fmsy_path,"/","forecast_updated.ss"))
writeLines(lines,paste0(model_01_.75Fmsy_path,"/","forecast.ss"))


run(dir = model_01_.75Fmsy_path,
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")
## plotting
model_01_.75Fmsy = SS_output(model_01_.75Fmsy_path, forecast = TRUE)


############################
## Running retrospectives ##
############################

##copy_SS_inputs(dir.old = model_01_path, dir.new = "model_01_RA")
model_01_RA_path <- file.path(mydir, "model_01_RA")
run(dir = model_01_RA_path,
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")

## run the retrospective analyses
retro(
  dir = model_01_RA_path, # wherever the model files are
  oldsubdir = "", # subfolder within dir
  newsubdir = "retrospectives", # new place to store retro runs within dir
  years = 0:-5, # years relative to ending year of model
  exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe"
)


# load the 6 models
retroModels <- SSgetoutput(dirvec = file.path( model_01_RA_path, "retrospectives", paste("retro", 0:-5, sep = "")))
# summarize the model results
retroSummary <- SSsummarize(retroModels)
# create a vector of the ending year of the retrospectives
endyrvec <- retroSummary[["endyrs"]] + 0:-5
# make plots comparing the 6 models
# showing 2 out of the 19 plots done by SSplotComparisons
par(mfrow = c(2, 2))
SSplotComparisons(retroSummary,
                  endyrvec = endyrvec,
                  legendlabels = paste("Data", 0:-5, "years"),
                  subplot = c(2,4,6,8,10,12,13,15,16,20,21), # only show one plot in vignette
                  print = TRUE, # send plots to PNG file
                  plot = FALSE, # don't plot to default graphics device
                  plotdir = model_01_RA_path )

rho_output <- SSmohnsrho(
  summaryoutput = retroSummary,
  endyrvec = endyrvec,
  startyr = retroSummary[["endyrs"]] - 5,
  verbose = FALSE
)






##########################################
## Running retrospectives  for model 08 ##
##########################################

##copy_SS_inputs(dir.old = model_08_path, dir.new = "model_08_RA")
model_08_RA_path <- file.path(mydir, "model_08_RA")
run(dir = model_08_RA_path,
    show_in_console = TRUE,
    skipfinished = F,
    exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe")

## run the retrospective analyses
retro(
  dir = model_08_RA_path, # wherever the model files are
  oldsubdir = "", # subfolder within dir
  newsubdir = "retrospectives", # new place to store retro runs within dir
  years = 0:-5, # years relative to ending year of model
  exe = "P:/pCloud Sync/_ss3-user-examples-main/model_files/toothfish/ss3_win.exe"
)


# load the 6 models
retroModels <- SSgetoutput(dirvec = file.path( model_08_RA_path, "retrospectives", paste("retro", 0:-5, sep = "")))
# summarize the model results
retroSummary <- SSsummarize(retroModels)
# create a vector of the ending year of the retrospectives
endyrvec <- retroSummary[["endyrs"]] + 0:-5
# make plots comparing the 6 models
# showing 2 out of the 19 plots done by SSplotComparisons
par(mfrow = c(2, 2))
SSplotComparisons(retroSummary,
                  endyrvec = endyrvec,
                  legendlabels = paste("Data", 0:-5, "years"),
                  subplot = c(2,4,6,8,10,12,13,15,16,20,21), # only show one plot in vignette
                  print = TRUE, # send plots to PNG file
                  plot = FALSE, # don't plot to default graphics device
                  plotdir = model_08_RA_path )

rho_output <- SSmohnsrho(
  summaryoutput = retroSummary,
  endyrvec = endyrvec,
  startyr = retroSummary[["endyrs"]] - 5,
  verbose = FALSE
)




# $SSB
# [1] -0.9115363
# 
# $Rec
# [1] -0.1216595
# 
# $Bratio
# [1] -0.8039301
# 
# $F
# [1] 1.973765
# 
# $WoodHole_SSB.all
# [1] -0.9229769
# 
# $WoodHole_Rec.all
# [1] 0.4077414
# 
# $WoodHole_Bratio.all
# [1] -0.8157475
# 
# $WoodHole_F.all
# [1] 1.836572
# 
# $AFSC_Hurtado_SSB
# [1] -0.1823073
# 
# $AFSC_Hurtado_Rec
# [1] -0.0243319
# 
# $AFSC_Hurtado_F
# [1] 0.3947531
# 
# $AFSC_Hurtado_Bratio
# [1] -0.160786
