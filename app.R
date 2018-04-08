library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)


# devtools::install_github("ITHIM/ITHIM", ref="devel", force=TRUE)
library("ITHIM")

# PAfilePath <- "https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/activeTransportTime.csv"
# BURfilePath <- "https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/burden.portland.csv"
# POPfilePath <- "https://raw.githubusercontent.com/ITHIM/ITHIM/devel/inst/F.portland.csv"

PAexamplePath <- system.file("activeTravelOHAS.csv", package = "ITHIM")
BURexamplePath <- system.file("burden.portland.csv", package = "ITHIM")
POPexamplePath <- system.file("F.portland.11_21_2017.csv", package = "ITHIM")

PAdownload <- read.csv(PAexamplePath, header=T)
BURdownload <- read.csv(BURexamplePath, header=T)
POPdownload <- read.csv(POPexamplePath, header=T)




ui <-  fluidPage(
  navbarPage(position = "fixed-top", 
             header = tags$style(type="text/css", "body {padding-top: 70px;}"), 
             theme = shinytheme(theme = "simplex"),
             title = "ITHIMio",
             
             
             tabPanel(title = "Introduction",
                      column(6, 
                             includeHTML("intro.html")
                             )
             ),
             
             navbarMenu(title = "Inputs",
               
               tabPanel("Physical Activity",
                      fluidRow(column(3, 
                                        ####### PHYSICAL ACTIVITY ########

                                        # Download Button
                                        downloadLink(outputId = "downloadPAexample", label = "Download Sample Transport Times"), 
                                        br(),
                                        # Upload PA Data
                                        fileInput('file1', 'Choose Physical Activity File (csv)',
                                                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                                      ),
                               column(9, 
                                      wellPanel(
                                        plotOutput('PAplot')
                                      )
                                      )
                      ),
                      fluidRow(
                        wellPanel(
                          dataTableOutput('PAtab')
                        )
                      )
               ),
               tabPanel("Disease Burden",
                        fluidRow(column(3, 
                                        ####### BURDEN ########
                                        # Download Button
                                        downloadLink("downloadBURexample", "Download Sample Disease Burdens"),
                                        br(),
                                        # Upload burden Data
                                        fileInput('file2', 'Choose Disease Burden File (csv)',
                                                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                        ),
                        column(9, 
                               wellPanel(
                                 plotOutput('BURplot')
                               )
                        )
                        ),
                        fluidRow(
                          wellPanel(
                            dataTableOutput('BURtab')
                          )
                        )
               ),
               tabPanel("Population",
                        fluidRow(column(3, 
                                       
                                        ####### POP ########
                                        # Download Button
                                        downloadLink("downloadPOPexample", "Download Sample Populations"),
                                        br(),
                                        # Upload Pop Data
                                        fileInput('file3', 'Choose Population File (csv)',
                                                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')) # ,
                                        # 
                                        # fileInput('fileXL', 'Or, upload an excel version with a single calibration sheet (in Beta)',
                                        #           accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))     
                                        
                                        
                        ),
                        column(9, 
                               wellPanel(
                                 plotOutput('POPplot')
                               )
                        )
                        ),
                        fluidRow(
                          wellPanel(
                            dataTableOutput('POPtab')
                          )
                        )
               )
               ), # close NavBar Menu
             
             
             tabPanel(title = "Scenario",
                      fluidRow(
                        column(3, 
                               sliderInput(inputId = "newWalk", label = "% Increase in Mean Walking Time",value = 0, step = 5,round = T,max = 100, min = 0)
                        
                              ),
                        column(3, 
                               sliderInput(inputId = "newCycle", label = "% Increase in Mean  Cycling Time", value = 0, step = 5,round = T, max = 100, min = 0)
                               )
                      )
             ),
             
             
             tabPanel(title = "Results",
                      fluidRow(
                        column(3, 
                               tags$h4(textOutput('summary'))
                               ),
                        column(9, 
                               wellPanel(dataTableOutput('superTab'))
                      
                      )
                      ),
                      wellPanel(plotOutput("resultsPlot"))
             )
  )
)

             
             
     

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
  
  ITHIM.double <- reactive({
    
    update(ITHIM.baseline(), 
           list(muwt = ITHIM.baseline()@parameters@muwt*2, 
                muct = ITHIM.baseline()@parameters@muct*2))
    
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
                                       ". \n Under the suggested scenario: \n mean walking time increased by ",input$newWalk,
                                       "% (to ", round(newWalk(),0)," min/week) \n mean cycling time increased by ",input$newCycle,
                                       "% (to ", round(newCycle(),0)," min/week). \n This would prevent an additional ",lives.scenario," deaths per year.")
  })
  
  
  
  superTable <- eventReactive(c(input$newWalk, input$newCycle),
    
    
    {
      superTabulate(ITHIM.baseline = ITHIM.baseline(), ITHIM.scenario.list = c(ITHIM.scenario(), ITHIM.double()))
      }
    )
  
  output$superTab <- renderDataTable({
    
    superTable()
    
  })
  
  
  output$resultsPlot <- renderPlot({
    
    superTable() %>%
      filter(burdenType == "daly") %>%
      spread(key = vision, value = value) %>%
      mutate(difference = scenario1-scenario2) %>%
      group_by(disease, ageClass) %>%
      summarise(avoidedBurden = sum(difference, na.rm=T)) %>%
        ggplot(aes(x = ageClass, y = avoidedBurden, fill = disease)) +
      geom_bar(stat = "identity")
    
    
    
  })
  
}


shinyApp(server = server, ui = ui)
