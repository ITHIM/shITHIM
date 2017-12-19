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


ui <- shinyUI(pageWithSidebar(
headerPanel("ITHIM Physical Activity Module Demo"),
sidebarPanel(
  
  
  sliderInput(inputId = "newWalk", label = "% Increase in Mean Walking Time",value = 0, step = 5,round = T,max = 100, min = 0),
  sliderInput(inputId = "newCycle", label = "% Increase in Mean  Cycling Time", value = 0, step = 5,round = T, max = 100, min = 0),
  
  
  ####### PHYSICAL ACTIVITY ########
  
  # Download Button
  downloadLink(outputId = "downloadPAexample", label = "Download Sample Transport Times"),
  # Upload PA Data
  fileInput('file1', 'Choose Physical Activity File (csv)',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
 
  
  ####### BURDEN ########
  # Download Button
  downloadLink("downloadBURexample", "Download Sample Disease Burdens"), 
  # Upload burden Data
  fileInput('file2', 'Choose Disease Burden File (csv)',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),

 
  
  ####### POP ########
  # Download Button
  downloadLink("downloadPOPexample", "Download Sample Populations"),
  # Upload Pop Data
  fileInput('file3', 'Choose Population File (csv)',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
  
  fileInput('fileXL', 'Or, upload an excel version with a single calibration sheet (in Beta)',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
  
),
mainPanel(
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              tabPanel("Introduction", includeHTML("intro.html")),
              tabPanel("Input - Physical Activity", plotOutput('PAplot'), dataTableOutput('PAtab')),
              tabPanel("Input - Disease Burden", plotOutput('BURplot'), dataTableOutput('BURtab')),
              tabPanel("Input - Population",   plotOutput('POPplot'), dataTableOutput('POPtab')), 
              tabPanel("Output - Summary", 
                       textOutput('summary'), 
                       hr(), 
                       textOutput('baseline.sum'), 
                       hr(),
                       textOutput('scenario.sum'))
  )
  
  )
))


server <- function(input, output, session) {
  
  ##### PHYSICAL ACTIVITY #######
  
  
  output$PAplot <- renderPlot({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  
  inFilePA <- input$file1
  
  
  PAfile <- read.csv(ifelse(is.null(inFilePA),PAexamplePath,inFilePA$datapath),header = T, sep=",")
  
    ggplot(PAfile, aes(x=ageClass, y=value, fill=sex)) + geom_bar(stat="identity", position="dodge") + 
    facet_grid(mode ~ .) +
    ggtitle("Age-Sex Active Transport Times") +
    ylab("Minutes per Week")

})
  
  
  output$PAtab <- renderDataTable({
    
    inFilePA <- input$file3
    
    PAfile <- read.csv(ifelse(is.null(inFilePA),PAexamplePath,inFilePA$datapath), header = T, sep=",") 
    
    PAfile
    
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadPAexample <- downloadHandler(
    filename = "ActiveTransportTime.csv",
    content = function(file) {
      write.csv(PAdownload, file, row.names = FALSE)
    }
  )
  
  ##### BURDEN ###########
  
  
  output$BURplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFileBurden <- input$file2
  
    burdenFile <- read.csv(ifelse(is.null(inFileBurden),BURexamplePath,inFileBurden$datapath), header = T, sep=",") 
    
      ggplot(burdenFile, aes(x=ageClass, y=value, fill=sex)) + geom_bar(stat="identity", position="dodge") + 
      facet_grid(burdenType ~ disease) +
      ggtitle("Age-Sex Baseline Disease Burdens") 
  })
  
  
  output$BURtab <- renderDataTable({
    
    inFileBUR <- input$file3
    
    BURfile <- read.csv(ifelse(is.null(inFileBUR),BURexamplePath,inFileBUR$datapath), header = T, sep=",") 
    
    BURfile
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadBURexample <- downloadHandler(
    filename = "PortlandBurden.csv",
    content = function(file) {
      write.csv(BURdownload, file, row.names = FALSE)
    }
  )
  
  
  
  ##### POPULATION ###########
  
  
  output$POPplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
   inFilePOP <- input$file3
    
   POPfile <- read.csv(ifelse(is.null(inFilePOP),POPexamplePath,inFilePOP$datapath), header = T, sep=",") 
   
      ggplot(POPfile, aes(x=ageClass, y=value, fill=sex)) + geom_bar(stat="identity", position="dodge") + 
      ggtitle("Age-Sex Population Distribution") 
  })
  
  output$POPtab <- renderDataTable({
    
    inFilePOP <- input$file3
    
    POPfile <- read.csv(ifelse(is.null(inFilePOP),POPexamplePath,inFilePOP$datapath), header = T, sep=",") 
    
    POPfile
  
    
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadPOPexample <- downloadHandler(
    filename = "PortlandPopulation.csv",
    content = function(file) {
      write.csv(POPdownload, file, row.names = FALSE)
    }
  )
  
  
  ##### summary in create ITHIM File ###########
  
  ITHIM.baseline <- reactive({
    
    inFilePA <- ifelse(is.null(input$file1), PAexamplePath, input$file1$datapath)
    inFileBUR <- ifelse(is.null(input$file2), BURexamplePath, input$file2$datapath)
    inFilePOP <- ifelse(is.null(input$file3), POPexamplePath, input$file3$datapath)
    
    
    return(createITHIM(activeTransportFile = inFilePA, GBDFile = inFileBUR, FFile = inFilePOP))
    
  })
  
    
  
newWalk <- reactive({ITHIM.baseline()@parameters@muwt*((input$newWalk/100)+1)})
newCycle <- reactive({ITHIM.baseline()@parameters@muct*((input$newCycle/100)+1)})   
    
ITHIM.scenario <- reactive({

  update(ITHIM.baseline(), list(muwt = newWalk(), muct = newCycle()))

    })
    
  output$summary <- renderText({
    
    lives0 <- round(deltaBurden(ITHIM.baseline(), update(ITHIM.baseline(), list(muwt = 0.1,muct = 0.1)), bur = "deaths" , dis = "all"),0)
    
    lives.scenario <- round(deltaBurden(ITHIM.baseline(), ITHIM.scenario(), bur = "deaths" , dis = "all"),0) *-1
    
    paste0("The estimated number of deaths prevented by current walking and cycling levels is ", lives0,
                                       ". Under the suggested scenario, mean walking time increased by ",input$newWalk,
                                       "% (to ", round(newWalk(),0)," min/week) and mean cycling time increased by ",input$newCycle,
                                       "% (to ", round(newCycle(),0)," min/week). This would prevent an additional ",lives.scenario," deaths per year.")
  })
  
  # output$baseline.sum <- dataITHIM()$ITHIM.baseline
  # 
  # output$scenario.sum <- dataITHIM()$ITHIM.scenario
  
  
}


shinyApp(server = server, ui = ui)
