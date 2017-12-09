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
  
  ####### PHYSICAL ACTIVITY ########
  
  # Download Button
  downloadLink("downloadPAexample", "Download Sample Transport Times"),
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
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
  
),
mainPanel(
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              tabPanel("Introduction", textOutput("This is a web interface for the Intergrated Transport Health Impact Model (ITHIM),")),
              tabPanel("Input - Physical Activity", plotOutput('PAplot'), dataTableOutput('PAtab')),
              tabPanel("Input - Disease Burden", plotOutput('BURplot'), dataTableOutput('BURtab')),
              tabPanel("Input - Population",   plotOutput('POPplot'), dataTableOutput('POPtab')), 
              tabPanel("Baseline - Summary", verbatimTextOutput('summary'))
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
  
  output$summary <- renderText({
    
    inFilePA <- ifelse(is.null(input$file1), PAexamplePath, input$file1$datapath)
    inFileBUR <- ifelse(is.null(input$file2), BURexamplePath, input$file2$datapath)
    inFilePOP <- ifelse(is.null(input$file3), POPexamplePath, input$file3$datapath)
    
    
    ITHIM.baseline <- createITHIM(activeTransportFile = inFilePA, GBDFile = inFileBUR, FFile = inFilePOP)
    ITHIM.2027.constrained <- update(ITHIM.baseline, list(muwt = 44.91, muct = 17.82))
    
    ITHIM.baseline
    
    ITHIM.2027.constrained
    
    deltaBurden(ITHIM.baseline, ITHIM.2027.constrained, bur = "daly", dis = "CVD")
    
    
  })
  
}

shinyApp(server = server, ui = ui)
