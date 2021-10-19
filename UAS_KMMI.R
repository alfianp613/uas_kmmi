library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(mathjaxr)

df1 = read.csv(url("https://raw.githubusercontent.com/alfianp613/uas_kmmi/main/Dataset/Real_Estate_Dataset.csv"),sep=";")
df2 = read.csv(url("https://raw.githubusercontent.com/alfianp613/uas_kmmi/main/Dataset/real_estate_dataset_no_norm.csv"))
df3 = read.csv(url("https://raw.githubusercontent.com/alfianp613/uas_kmmi/main/Dataset/real_estate_dataset_with_norm.csv"))


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
  dashboardBody(
    tags$head(tags$style(HTML('
        /* logo */
        .skin-black .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }'))),
    tabItems(
      # First tab content
      tabItem(skin = "black",
              tabName = "load_data",
              fluidPage(
                fluidRow(
                  column(4,
                  # Input: Select a file ----
                  selectInput("file1", "Pilih Dataset:",
                              choices = c(" ", 
                                          "Dataset Real Estate",
                                          "Dataset Real Estate Clean", 
                                          "Dataset Real Estate Clean with Normalization")),
                    ),
                column(8,
                  h5("Link Dataset: "),
                  tags$a(href="https://archive.ics.uci.edu/ml/datasets/Real+estate+valuation+data+set", target="_blank", "Sumber Dataset"),
                  tags$br(),
                  tags$a(href="https://drive.google.com/file/d/1f8hrvw7lmWOj5kxhyDM8fkDANcktb6bK/view?usp=sharing", target="_blank", "Dataset Real Estate (.csv)"),
                  tags$br(),
                  tags$a(href="https://drive.google.com/file/d/17zedL6yFQsrdpqLy-xZapTg0H-b4KzJK/view?usp=sharing", target="_blank", "Dataset Real Estate Clean"),
                  tags$br(),
                  tags$a(href="https://drive.google.com/file/d/1FQdcNeARSYQ2_nMdKM6GyA1JSolfRNZT/view?usp=sharing", target="_blank", "Dataset Real Estate Clean with Normalization"))),
                  tags$hr(),
                  DT::dataTableOutput("contents"),
                  # tableOutput("contents"),
                  tags$br(),
                  verbatimTextOutput("keterangan")
                )),
      # Second tab content
      tabItem(skin = "black",
              tabName = "visualisasi",
              fluidPage(tabsetPanel(type = "pills",
                            tabPanel("Plot", tabsetPanel(
                              type="tabs",
                              tabPanel("Histogram",
                                       tags$br(),
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
                                       tags$br(),
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
                              tabPanel("Time-Series Plot",
                                       tags$br(),
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
                                     tags$br(),
                                     h3("Summary Real Estate Data"),
                                     mainPanel(verbatimTextOutput("summary")),
                                     tags$head(tags$style("#summary{color: text-align: center;
                                                                                font-size: 17px;
                                                                                }")))))),
      tabItem(skin = "black",
              tabName = "analisis",
              fluidPage(
                tabsetPanel(type = "pills",
                            tabPanel("Regresi Linear",
                                     tags$br(),
                                     h3("Model Regresi Linear Real Estate Valuation"),
                                     mainPanel(verbatimTextOutput("model"),
                                               h3(textOutput("teksinter")),
                                               uiOutput("equation"),
                                               tags$pre(textOutput("isi")),
                                               tags$head(tags$style("#teksinter{color: black;
                                                                                font-size: 25px;
                                                                                text-align: left;
                                                                                }")),
                                               tags$head(tags$style("#isi{color: black;
                                                                                font-size: 20px;
                                                                                text-align: justify;
                                                                                }")),
                                               tags$head(tags$style("#equation{color: black;
                                                                                font-size: 20px;
                                                                                }")),
                                               tags$head(tags$style("#model{color: text-align: center;
                                                                                font-size: 17px;
                                                                                }")))),
                            tabPanel("Prediksi",
                                     tags$br(),
                                     sidebarPanel(
                                       numericInput("X2", "Umur rumah (tahun)", 0),
                                       numericInput("X3", "Jarak rumah ke stasiun MRT terdekat (m)", 0),
                                       numericInput("X4", "Jumlah toko serba ada di sekitar rumah", 0),
                                       numericInput("X5", "Koordinat geografis garis lintang (derajat)", 0),
                                       br(),
                                       actionButton("pred", "Prediksi")),
                                     mainPanel(
                                       h2("Hasil Prediksi:"),
                                       h3(textOutput("prediksi")),
                                       p(textOutput("catatan")),
                                       tags$head(tags$style("#prediksi{color: black;
                                                                                font-size: 50px;
                                                                                text-align: center;
                                                                                }")),
                                       tags$head(tags$style("#catatan{color: black;
                                                                                font-size: 25px;
                                                                                text-align: left;
                                                                                }"))))))),
      tabItem(tabName = "anggota",
              tags$br(),
              mainPanel(
                h1("Anggota Tim:"),
                tags$hr(),
                h3("162012133045 - Adam Maurizio W."),
                h3("162012133037 - M. Alfian Pratama"),
                h3("162012133013 - Denise Arne Ardanty"),
                h3("162012133016 - Nurul Aini"),
                tags$hr())))))

server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$file1,
           " "="",
           "Dataset Real Estate"=df1,
           "Dataset Real Estate Clean"=df2,
           "Dataset Real Estate Clean with Normalization"=df3)
  })
  output$contents = DT::renderDataTable({
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    datasetInput()
    
  })
  
  
  variableInput <- reactive({
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    df <- datasetInput()
    
    switch(input$variable,
           "House age (X2)"=df$X2,
           "Distance to the nearest MRT station (X3)"=df$X3,
           "Number of convenience stores (X4)"=df$X4,
           "Latitude (X5)"=df$X5,
           "Longitude (X6)"=df$X6,
           "Houses Price (Y)"=df$Y)
  })
  output$keterangan <- renderText({
    req(datasetInput())
    
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
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    df <- datasetInput()
    
    ggplot(df,aes(x=variableInput()))+
      geom_histogram(fill=colorInput(),
                     color="white",bins=input$bins)+
      labs(x=input$variable)
  },height = 375,width = 800)
  
  output$summary <- renderPrint({
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    df <- datasetInput()
    
    df$X1 = as.Date(df$X1)
    
    summary(df)})
  
  variableXInput <- reactive({
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    df <- datasetInput()
    
    switch(input$varx,
           "House age (X2)"=df$X2,
           "Distance to the nearest MRT station (X3)"=df$X3,
           "Number of convenience stores (X4)"=df$X4,
           "Latitude (X5)"=df$X5,
           "Longitude (X6)"=df$X6,
           "Houses Price (Y)"=df$Y)
  })
  
  variableYInput <- reactive({
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    df <- datasetInput()
    
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
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    df <- datasetInput()
    
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
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    df <- datasetInput()
    
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
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    df <- datasetInput()
    
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
    paste("Time-Series Plot",input$variable,sep=" ")
  })
  
  output$model <- renderPrint({
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    df <- datasetInput()
    
    df_reg1 = df[, c(3,4,5,6,8)]
    model1 <- lm(Y ~ ., data = df_reg1)
    summary(model1)
    })
  output$teksinter <- renderText({
    req(input$file1)
    "Interpretasi:"
  })
  
  datapred <- eventReactive(input$pred, {
    if (datasetInput()==df3) {
      dfs = df2[, c(3,4,5,6)]
      dfn = data.frame(X2 = input$X2,
                       X3 = input$X3,
                       X4 = input$X4,
                       X5 = input$X5)
      
      normalize <- function(x,df) {
        return ((x - min(df)) / (max(df) - min(df)))
      }
      
      normalize(dfn,dfs)
    } else {
      data.frame(X2 = input$X2,
                 X3 = input$X3,
                 X4 = input$X4,
                 X5 = input$X5)
    }
  })
  
  output$prediksi <- renderText({
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    df <- datasetInput()
    
    df_reg1 = df[, c(3,4,5,6,8)]
    model1 <- lm(Y ~ ., data = df_reg1)
    
    pred = predict(model1, datapred()) * 10000 
    paste(round(pred,3),"New Taiwan Dollar/Ping")
  })
  output$equation <- renderUI({
    validate(
      need(datasetInput() != "", "Silahkan pilih dataset terlebih dahulu!")
    )
    
    df <- datasetInput()
    
    df_reg1 = df[, c(3,4,5,6,8)]
    model1 <- lm(Y ~ ., data = df_reg1)
    
    modelSummary = summary(model1)
    modelCoeffs = modelSummary$coefficients
    
    plus <- function(x) {
      ifelse(x < 0,return(""),return("+"))
    }
    
    withMathJax(
      helpText(paste('Didapatkan persamaan regresi sebagai berikut: 
                     $$\\hat{Y} =',
                      round(modelCoeffs["(Intercept)","Estimate"],4),plus(modelCoeffs["X2","Estimate"]),
                      round(modelCoeffs["X2","Estimate"],4),"X_2",plus(modelCoeffs["X3","Estimate"]),
                      round(modelCoeffs["X3","Estimate"],4),"X_3",plus(modelCoeffs["X4","Estimate"]),
                      round(modelCoeffs["X4","Estimate"],4),"X_4",plus(modelCoeffs["X5","Estimate"]),
                      round(modelCoeffs["X5","Estimate"],4),'X_5$$',sep=""))
    )
  })
  output$isi <- renderText({
    req(datasetInput())
    
      'Keterangan:
      \nX2\t: House age
      \nX3\t: Distance to the nearest MRT station
      \nX4\t: Number of convenience stores
      \nX5\t: Latitude
      \nY\t: Houses Price'
  })
  
  output$catatan <- renderText({
    req(datasetInput())
    "Catatan: 1 Ping = 3.3 meter squared"
  })
}
shinyApp(ui, server)
  
