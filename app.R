library(shiny)
library(tidyverse)


#devtools::install_github("ITHIM/ITHIM", ref="devel")
#library("ITHIM")

PAexample <- read.csv("https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/activeTransportTime.csv", header=T)
GBDexample <- read.csv("https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/burden.portland.csv", header=T)
POPexample <- read.csv("https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/F.portland.csv", header=T)


ui <- shinyUI(pageWithSidebar(
headerPanel("ITHIM Physical Activity Module Demo"),
sidebarPanel(
  
  ####### PHYSICAL ACTIVITY ########
  
  # Upload PA Data
  fileInput('file1', 'Choose PhysActivity File (csv)',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
  
  # Download Button
  downloadButton("downloadPAexample", "Download Sample Transport Times"),

  ####### BURDEN ########
  
  # Upload burden Data
  fileInput('file2', 'Choose Disease Burden File (csv)',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
  
  # Download Button
  downloadButton("downloadGBDexample", "Download Sample Disease Burdens"), 
  
  
  ####### POP ########
  
  # Upload Pop Data
  fileInput('file3', 'Choose Population File (csv)',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
  
  # Download Button
  downloadButton("downloadPOPexample", "Download Sample Populations")
  
),
mainPanel(
  plotOutput('PA'),
  plotOutput('burden'),
  plotOutput('pop')
 
  
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
  output$downloadGBDexample <- downloadHandler(
    filename = "PortlandBurden.csv",
    content = function(file) {
      write.csv(GBDexample, file, row.names = FALSE)
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
      write.csv(GBDexample, file, row.names = FALSE)
    }
  )
  
  
  
 # createITHIM(activeTransportFile = PAfile, GBDFile = burdenFile, FFile = POPfile)
  
  
}




shinyApp(server = server, ui = ui)
