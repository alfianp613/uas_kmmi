library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggplot2)


ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Kelompok I"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Load Data", tabName = "load_data", icon = icon("upload")),
      menuItem("Visualisasi", tabName = "visualisasi", icon = icon("chart-bar")),
      menuItem("Analisis", tabName = "analisis", icon = icon("chart-line")),
      menuItem("Anggota", tabName = "anggota", icon = icon("users"))
    )
  ),
  ## Body content
  dashboardBody(skin = "black",
    tabItems(
      # First tab content
      tabItem(tabName = "load_data",
              sidebarLayout(
                sidebarPanel(
                  # Input: Select a file ----
                  fileInput("file1", "Masukkan Data real_estate_dataset_clean.csv",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  
                  # Horizontal line ----
                  tags$hr(),
                  
                  # Input: Checkbox if file has header ----
                  checkboxInput("header", "Header", TRUE),
                  
                  # Input: Select separator ----
                  radioButtons("sep", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ","),
                  
                  # Input: Select quotes ----
                  radioButtons("quote", "Quote",
                               choices = c(None = "",
                                           "Double Quote" = '"',
                                           "Single Quote" = "'"),
                               selected = '"'),
                  
                  # Horizontal line ----
                  tags$hr(),
                  
                  # Input: Select number of rows to display ----
                  radioButtons("disp", "Display",
                               choices = c(Head = "Head",
                                           Tail = "Tail",
                                           All = "all"),
                               selected = "Head")
                ),
                mainPanel(
                  h5("Link Dataset: "),
                  tags$a(href="https://archive.ics.uci.edu/ml/datasets/Real+estate+valuation+data+set", "Dataset Real Estate"),
                  tags$hr(),
                  tableOutput("contents"),
                  verbatimTextOutput("keterangan")
                ))
      ),
      # Second tab content
      tabItem(tabName = "visualisasi",
              mainPanel(
                tabsetPanel(type = "pills",
                            tabPanel("Plot", tabsetPanel(
                              type="tabs",
                              tabPanel("Histogram",
                                       sidebarPanel(
                                         
                                         selectInput(inputId = "variable",
                                                     label = "Pilih Variabel:",
                                                     choices = c("House age (X2)",
                                                                 "Distance to the nearest MRT station (X3)",
                                                                 "Number of convenience stores (X4)",
                                                                 "Latitude (X5)",
                                                                 "Longitude (X6)",
                                                                 "Houses Price (Y)")),
                                         selectInput(inputId = "color",
                                                     label = "Pilih Warna:",
                                                     choices = c("Dark Orange","Navy Blue","Firebrick","Dark Green")),
                                         br(),
                                         sliderInput(inputId = "bins",
                                                     label = "Number of bins:",
                                                     min = 1,
                                                     max = 50,
                                                     value = 30)
                                       ),
                                       mainPanel(p(textOutput("judulhistogram")),
                                                 tags$head(tags$style("#judulhistogram{color: black;
                                                    font-size: 20px;
                                                    text-align: left;
                                                    text-style: bold;
                                                    }")),
                                                 plotOutput(outputId = "Histogram"))),
                              tabPanel("Scatter Plot",
                                       sidebarPanel(
                                         selectInput(inputId = "varx",
                                                     label = "Pilih Variabel X:",
                                                     choices = c("House age (X2)",
                                                                 "Distance to the nearest MRT station (X3)",
                                                                 "Number of convenience stores (X4)",
                                                                 "Latitude (X5)",
                                                                 "Longitude (X6)",
                                                                 "Houses Price (Y)")),
                                         
                                         selectInput(inputId = "vary",
                                                     label = "Pilih Variabel Y:",
                                                     choices = c("Houses Price (Y)",
                                                                 "House age (X2)",
                                                                 "Distance to the nearest MRT station (X3)",
                                                                 "Number of convenience stores (X4)",
                                                                 "Latitude (X5)",
                                                                 "Longitude (X6)"
                                                                 )),
                                         br(),
                                         selectInput(inputId = "colorscat",
                                                     label = "Pilih Warna:",
                                                     choices = c("Dark Orange","Navy Blue","Firebrick","Dark Green")),
                                         selectInput(inputId = "colorreg",
                                                     label = "Pilih Warna Garis Regresi:",
                                                     choices = c("Dark Orange","Navy Blue","Firebrick","Dark Green"))
                                       ),
                                       mainPanel(p(textOutput("judulscatter")),
                                                 tags$head(tags$style("#judulscatter{color: black;
                                                    font-size: 20px;
                                                    text-align:left;
                                                    text-style: bold;
                                                    }")),
                                                 plotOutput(outputId = "Scatterplot",click = "plot_click"),
                                                 verbatimTextOutput("info"),
                                                 p(verbatimTextOutput("korelasi")),
                                                 tags$head(tags$style("#korelasi{color: black;
                                                    font-size: 15px;
                                                    text-align: left;
                                                    }")),
                                                 tags$head(tags$style("#info{color: black;
                                                    font-size: 15px;
                                                    text-align: left;
                                                    }")))),
                              tabPanel("Line Chart",
                                       sidebarPanel(
                                         selectInput(inputId = "varl",
                                                     label = "Pilih Variabel:",
                                                     choices = c("House age (X2)",
                                                                 "Distance to the nearest MRT station (X3)",
                                                                 "Number of convenience stores (X4)",
                                                                 "Latitude (X5)",
                                                                 "Longitude (X6)",
                                                                 "Houses Price (Y)")),
                                         selectInput(inputId = "colorl",
                                                     label = "Pilih Warna:",
                                                     choices = c("Dark Orange","Navy Blue","Firebrick","Dark Green"))),
                                       mainPanel(p(textOutput("judulline")),
                                                 tags$head(tags$style("#judulline{color: black;
                                                    font-size: 20px;
                                                    text-align: left;
                                                    text-style: bold;
                                                    }")),
                                                 plotOutput(outputId = "line"))))),
                            tabPanel("Summary",
                                     h3("Summary Real Estate Data"),
                                     mainPanel(verbatimTextOutput("summary")))))),
      tabItem(tabName = "analisis",
              mainPanel(
                tabsetPanel(type = "pills",
                            tabPanel("Regresi", 
                                     tabsetPanel(type="tabs",
                                                 tabPanel("Model"),
                                                 tabPanel("Asumsi"),
                                                 tabPanel("Prediksi"))),
                            tabPanel("Kesimpulan dan Saran")))),
      tabItem(tabName = "anggota",
              mainPanel(
                h1("Anggota Tim:"),
                tags$hr(),
                h3("162012133045 - Adam Maurizio W."),
                h3("162012133037 - M. Alfian Pratama"),
                h3("162012133013 - Denise Arne Ardanty"),
                h3("162012133016 - Nurul Aini"),
                tags$hr())))))
server <- function(input, output) {
  output$contents <- renderTable({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "Head") {
      return(head(df))
    }
    else if(input$disp == "Tail") {
      return(tail(df))
    }
    else {
      return(df)
    }
    
  })
  variableInput <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    switch(input$variable,
           "House age (X2)"=df$X2,
           "Distance to the nearest MRT station (X3)"=df$X3,
           "Number of convenience stores (X4)"=df$X4,
           "Latitude (X5)"=df$X5,
           "Longitude (X6)"=df$X6,
           "Houses Price (Y)"=df$Y)
  })
  output$keterangan <- renderText({
    req(input$file1)
    paste0("Keterangan:
    \nX1\t: Transaction date
    \nX2\t: House age
    \nX3\t: Distance to the nearest MRT station
    \nX4\t: Number of convenience stores
    \nX5\t: Latitude
    \nX6\t: Longitude
    \nY\t: Houses Price")
  })
  
  colorInput <- reactive({
    switch(input$color,
           "Dark Orange" = "darkorange",
           "Navy Blue" = "navyblue",
           "Firebrick" = "firebrick",
           "Dark Green" = "darkgreen")
  })
  output$Histogram <- renderPlot({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    ggplot(df,aes(x=variableInput()))+
      geom_histogram(fill=colorInput(),
                     color="white",bins=input$bins)+
      labs(x=input$variable)
  },height = 375,width = 800)
  
  output$summary <- renderPrint({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    summary(df)})
  
  variableXInput <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    switch(input$varx,
           "House age (X2)"=df$X2,
           "Distance to the nearest MRT station (X3)"=df$X3,
           "Number of convenience stores (X4)"=df$X4,
           "Latitude (X5)"=df$X5,
           "Longitude (X6)"=df$X6,
           "Houses Price (Y)"=df$Y)
  })
  
  variableYInput <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    switch(input$vary,
           "Houses Price (Y)"=df$Y,
           "House age (X2)"=df$X2,
           "Distance to the nearest MRT station (X3)"=df$X3,
           "Number of convenience stores (X4)"=df$X4,
           "Latitude (X5)"=df$X5,
           "Longitude (X6)"=df$X6)
  })
  
  colorscatInput <- reactive({
    switch(input$colorscat,
           "Dark Orange" = "darkorange",
           "Navy Blue" = "navyblue",
           "Firebrick" = "firebrick",
           "Dark Green" = "darkgreen")
  })
  colorregInput <- reactive({
    switch(input$colorreg,
           "Dark Orange" = "darkorange",
           "Navy Blue" = "navyblue",
           "Firebrick" = "firebrick",
           "Dark Green" = "darkgreen")
  })
  
  output$Scatterplot <- renderPlot({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    ggplot(df,
           aes(x=variableXInput(),
               y=variableYInput()))+
      geom_point(color=colorscatInput(),size=5)+
      geom_smooth(method="lm",color=colorregInput())+
      scale_y_continuous(labels = scales::comma)+
      labs(x=input$varx,
           y=input$vary)
  },height = 375,width = 800)
  
  varilineInput <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    switch(input$varl,
           "House age (X2)"=df$X2,
           "Distance to the nearest MRT station (X3)"=df$X3,
           "Number of convenience stores (X4)"=df$X4,
           "Latitude (X5)"=df$X5,
           "Longitude (X6)"=df$X6,
           "Houses Price (Y)"=df$Y)
  })
  colorlineInput <- reactive({
    switch(input$colorl,
           "Dark Orange" = "darkorange",
           "Navy Blue" = "navyblue",
           "Firebrick" = "firebrick",
           "Dark Green" = "darkgreen")
  })
  
  output$line <- renderPlot({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    df$X1 = as.Date(df$X1)
    
    aggr_df=aggregate(varilineInput(), by=df["X1"], FUN=mean)
    
    ggplot(aggr_df, aes(x=X1, y=unlist(aggr_df[2]))) +
      geom_line(color=colorlineInput(), size=2, alpha=0.9)+
      geom_point(size=2)+
      labs(x="Transaction Date",
           y=input$varl)
  },height = 375,width = 800)
  
  output$judulhistogram <- renderText({
    paste("Histogram",input$variable,sep=" ")
  })
  output$judulscatter <- renderText({
    paste("Scatter Plot",input$varx,"vs",input$vary,sep=" ")
  })
  output$korelasi <- renderText({
    paste("Koefieisn korelasi",input$varx,"~",input$vary,":",cor(variableXInput(),variableYInput()),sep=" ")
  })
  output$info = renderText({ 
    paste0("x : ", input$plot_click$x, "\ny : ", input$plot_click$y) 
  })
  output$judulline <- renderText({
    paste("Tren",input$variable,sep=" ")
  })
}
shinyApp(ui, server)

