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