library(shiny)
library(tidyverse)


devtools::install_github("ITHIM/ITHIM", ref="devel")
library("ITHIM")

PAfileName <- "https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/activeTransportTime.csv"
BURfileName <- "https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/burden.portland.csv"
POPfileName <- "https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/F.portland.csv"

PAexample <- read.csv(PAfileName, header=T)
BURexample <- read.csv(BURfileName, header=T)
POPexample <- read.csv(POPfileName, header=T)


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
              tabPanel("Input - Physical Activity", plotOutput('PA')),
              tabPanel("Input - Disease Burden", plotOutput('burden')),
              tabPanel("Input - Population",   plotOutput('pop')), 
              tabPanel("Baseline - Summary", verbatimTextOutput('summary'))
  )
  
  )
))


server <- function(input, output, session) {
  
  ##### PHYSICAL ACTIVITY #######
  
  output$PA <- renderPlot({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  
  inFilePA <- input$file1
  
  if (is.null(inFilePA))
    return(NULL)
  
  PAfile <- read.csv(inFilePA$datapath, header = T, sep=",")
  
    ggplot(PAfile, aes(x=ageClass, y=value, fill=sex)) + geom_bar(stat="identity", position="dodge") + 
    facet_grid(mode ~ .) +
    ggtitle("Age-Sex Active Transport Times") +
    ylab("Minutes per Week")
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadPAexample <- downloadHandler(
    filename = "ActiveTransportTime.csv",
    content = function(file) {
      write.csv(PAexample, file, row.names = FALSE)
    }
  )
  
  ##### BURDEN ###########
  
  
  output$burden <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFileBurden <- input$file2
    
    if (is.null(inFileBurden))
      return(NULL)
    
    burdenFile <- read.csv(inFileBurden$datapath, header = T, sep=",") 
    
      ggplot(burdenFile, aes(x=ageClass, y=value, fill=sex)) + geom_bar(stat="identity", position="dodge") + 
      facet_grid(burdenType ~ disease) +
      ggtitle("Age-Sex Baseline Disease Burdens") 
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadBURexample <- downloadHandler(
    filename = "PortlandBurden.csv",
    content = function(file) {
      write.csv(BURexample, file, row.names = FALSE)
    }
  )
  
  
  
  ##### POPULATION ###########
  
  
  output$pop <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFilePOP <- input$file3
    
    if (is.null(inFilePOP))
      return(NULL)
    
   POPfile <- read.csv(inFilePOP$datapath, header = T, sep=",") 
   
      ggplot(POPfile, aes(x=ageClass, y=value, fill=sex)) + geom_bar(stat="identity", position="dodge") + 
      ggtitle("Age-Sex Population Distribution") 
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadPOPexample <- downloadHandler(
    filename = "PortlandPopulation.csv",
    content = function(file) {
      write.csv(BURexample, file, row.names = FALSE)
    }
  )
  
  
  
  ##### summary in create ITHIM File ###########
  
  output$summary <- renderText({
    
    inFilePA <- ifelse(is.null(input$file1), PAexample)
    inFileBUR <- ifelse(is.null(input$file2), BURexample)
    inFilePOP <- ifelse(is.null(input$file3), POPexample)
    
    if (is.null(inFilePA) | is.null(inFileBUR) | is.null(inFilePOP))
      return(NULL)

    # baseline <- createITHIM(activeTransportFile = inFilePA, GBDFile = inFileBUR, FFile = inFilePOP) 
    
    baseline <- createITHIM(activeTransportFile = PAfileName, GBDFile = BURfileName, FFile = POPfileName)
    
    print(baseline)
    
  })
  
}

shinyApp(server = server, ui = ui)
