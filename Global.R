library(shiny)
library(tidyverse)


devtools::install_github("ITHIM/ITHIM", ref="devel")
library("ITHIM")

# PAfilePath <- "https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/activeTransportTime.csv"
# BURfilePath <- "https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/burden.portland.csv"
# POPfilePath <- "https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/F.portland.csv"

PAexamplePath <- system.file("activeTravelOHAS.csv", package = "ITHIM")
BURexamplePath <- system.file("gbd_Manuscript_2011-2015.csv", package = "ITHIM")
POPexamplePath <- system.file("F.portland.11_21_2017.csv", package = "ITHIM")

PAdownload <- read.csv(PAexamplePath, header=T)
BURdownload <- read.csv(BURexamplePath, header=T)
POPdownload <- read.csv(POPexamplePath, header=T)


ITHIM.demo.summary <- function(PAin = "inFilePA", BURin = "inFileBUR",  POPin = "inFilePOP", WALKin = 0, CYCin = 0){

  ITHIM.baseline <- createITHIM(activeTransportFile = inFilePA, GBDFile = inFileBUR, FFile = inFilePOP)
  startWalk <- ITHIM.baseline@parameters@muwt
  startCycle <- ITHIM.baseline@parameters@muct
  newWalk <- startWalk*((WALKin/100)+1)
  newCycle <- startCycle*((CYCin/100)+1)
  
  ITHIM.walk0 <- update(ITHIM.baseline, list(muwt = 0.1,muct = 0.1))
  
  lives0 <- round(deltaBurden(ITHIM.baseline, ITHIM.walk0, bur = "deaths" , dis = "all"),0)
  ITHIM.scenario <- update(ITHIM.baseline, list(muwt = newWalk, 
                                                muct = newCycle))
  lives.scenario <- round(deltaBurden(ITHIM.baseline, ITHIM.scenario, bur = "deaths" , dis = "all"),0)
  
  
  ITHIM.scenario <- update(ITHIM.baseline, list(muwt = newWalk, 
                                                muct = newCycle))
  lives.scenario <- round(deltaBurden(ITHIM.baseline, ITHIM.scenario, bur = "deaths" , dis = "all"),0) *-1
  
  statement <- paste0("The estimated number of deaths prevented by current walking and cycling levels is ", lives0,
                      ". Under the suggested scenario, mean walking time increased by ",WALKin,
                      "% (to ", round(newWalk,0)," min/week) and mean cycling time increased by ",CYCin,
                      "% (to ", round(newCycle,0)," min/week). This would prevent an additional ",lives.scenario," deaths per year.")
  
  return(statement)
}