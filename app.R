#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



library(tidyverse) # Data management and manipulation
library(ggplot2) # Visuals


#for k means
library(ggthemes)
library(ggpubr)
library(grid)
library(gridExtra)




rsconnect::setAccountInfo(name='optiver7', token='B7D2C396DC6FCD9C5055C223F7C9E4DF', secret='1alZ5PsFaC0pckcgXQ0szSZ+w7VboQ6CstLiSlg/')


data1 <- read.csv("lightgbm_baseline_predict.csv")
data2 <- read.csv("all_times.csv")
data3 <- read.csv("final-ensemble.csv")
data4 <- read.csv("dummy.csv")
data4$stock_id <- gsub("stock_", "", data4$stock_id)






# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "superhero"),
  
  #add main button
  navbarPage("Optiver7",
             #first
             navbarMenu("Guide", 
                        tabPanel("Introduction",
                                 div(style='width:1400px; height:300px',
                                     br(),
                                     
                                     titlePanel("Welcome to Optiver Dashboard!"),
                                     br(),
                                     h3("Motivation"),
                                     HTML("Our motivation stemmed from our financial understanding of volatility and using this to guide the features of a stock <br>which we believed would best represent its volatility." ),
                                     br(),
                                     h3("Who is for?"),
                                     HTML("Our main target audience is those engaged in the financial services industry with our tool being useful for a range of <br> people in different roles.
                                            These roles include trading, asset management, hedge fund managers, equity research, brokerage <br> and financial consultants.
                                            Our app is also applicable for the use of academics and students, and for anyone who is looking into <br> the role of data science in trading."),
                                     br(),
                                     h3("How to use this app?"),
                                     HTML("Since our audience is quite broad, our expectation for the app is to be as simple and convenient as possible, so that users can <br> search for effective information quickly. The format is simple and easier for users to use.")

                                     
                                 
                                 )
                            
                            ),
                        tabPanel("Aim",
                                 br(),
                                 br(),
                                 tags$p("We sought to capture volatility across 126 different stocks in different time intervals, predicting volatility through a range of time intervals. 
                                           We visualised our results in the app to show how we can predict volatility of these stocks in a certain period of time.
                                           We optimised our clustering through a tradeoff between accuracy and explainability to create our features.
                                           We represented communication through displaying both the stock features and their volatility predictions in the Shiny App.
                                           This app will show traders how different models and tools have been used to generate our predictions.",style = "font-size: 20px; "),
                                 imageOutput("photo1")

                        
                                 )
                            ),
             
             
             
            
             
             
             
             
             #second
             tabPanel("Data processing",
                      fluidRow(
                        column(4, selectInput("time_id", "TIME ID",  unique(data2$time_id))),
                        column(4, selectInput("stock_id", "STOCK ID", unique(data2$stock_id)))
                        
                        ),
                      submitButton("Submit",width = "750px"),
                      
                      fluidRow(
                        column(3, h6("Beta", textOutput("beta"))),
                        column(3, h6("Spread", textOutput("spread"))),
                        column(3, h6("Dom", textOutput("dom")))),
                      
                      br(),
                      p("Beta, DOM(market depth), and spread are financial indicators that provide different information about a stock or financial market:"),
                      strong("Beta:"),
                      p("Beta is a measure of a stock's risk relative to the market. A beta of 1 indicates that the price of a stock moves with the market. A beta coefficient of less than 1 means a stock is less volatile than the market, while a beta of more than 1 means a stock is more volatile. Beta is very useful in portfolio construction, where one can balance between high beta stocks and low beta stocks to achieve an acceptable level of market risk."),
                      strong("DOM(Market depth): "),
                      p("Market depth is a measure of security supply and demand. It usually shows the number of open buy and sell orders for a security at different prices. The depth of market data can provide information about the liquidity of securities, and securities with large buy and sell orders at many price levels are considered more liquid. Liquidity is an important characteristic of traders because it describes the extent to which assets can be bought and sold quickly without affecting the price of the asset."),
                      strong("Spread: "),
                      p("The spread is the difference between the bid price (the highest price a buyer is willing to pay for an asset) and the ask price (the lowest price a seller is willing to accept). A narrower spread indicates a more liquid market, while a wider spread indicates a less liquid market. Spreads affect transaction costs (transaction costs), so they are of great interest to traders and investors.)" )
                      ),
                      
             
             
             
             
             
                      
             #third
             navbarMenu("LightGBM",
                        tabPanel("Forecasting",
                                 fluidRow(column(4,selectInput("t", "Time ID", unique(data1$time_id))),
                                          column(4,selectInput("s", "Stock ID", unique(data1$stock_id)))),
                                 submitButton("Submit",width = "750px"),
                                 fluidRow(column(4,h6("Target realized volatility", textOutput("target"))),
                                          column(4,h6("Predicted volatility", textOutput("predict"))),
                                          column(4,h6("RMSE", textOutput("rmse"))))
                                 ),
                        
                        tabPanel("Model Feature Importance",
                                 HTML("This figure illustrates the important features of the model by using lightBGM"),
                                 br(),
                                 fluidRow(
                                   column(7,imageOutput("photo2")),
                                   column(5,imageOutput("photo3")))),
                                
                        tabPanel("Top 25 Important Features",
                                 fluidRow(
                                   column(7,imageOutput("photo4")),
                                   column(5,imageOutput("photo5")))),
                        
                        tabPanel("Top 40 Important Features",
                                 fluidRow(
                                   column(7,imageOutput("photo6")),
                                   column(5,imageOutput("photo7"))))
                        ),
             
             
             #fifth
             tabPanel("Forecasting",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("tt", "Time ID", data3$time_id),
                          selectInput("ss", "Stock ID", data3$stock_id),
                          selectInput("model", "Select model to show", choices = c("LGBM","POLY","ENSEBLES")),
                          
                          uiOutput("select")
                         
                          
                        ),
                        mainPanel(
                          plotOutput(" ")
                        )
                      )
                      
                      
                      ),
             
            
             
             
             
             #sixth
             tabPanel("NO",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("stock1", "Stock ID", choices = unique(data1$stock_id)),
                          #numericRangeInput("range","Range",min = min(unique(data1$time_id)), max = max(unique(data1$time_id)),
                                      #value = c(min(unique(data1$time_id)),max(unique(data1$time_id)))),
                          numericInput("start","Start Time",value = 5, min = min(unique(data1$time_id)), max = max(unique(data1$time_id)),step = 3),
                          numericInput("end","End Time",value = 725, min = min(unique(data1$time_id)),max = max(unique(data1$time_id)),step = 3),
                          submitButton("Show"),
                          br(),
                          p("⚠️ You can select any stock and select the corresponding time range, noting that the start time should not be less than the end time, and then click the button, you can make a prediction of the trend of the stock")
                         
                          
                          
                        ),
                        mainPanel(
                          plotOutput("plot",height = "600px")
                          
                        )
                      )
               
             )
          
             
             

             
  )
)





# Define server logic required to draw a histogram
server <- function(input, output) {

  
  
  output$photo1 <- renderImage({
    list(
      src = "1.png",
      filetype = "image/png",
      width = 700,
      height = 300,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  
  

  output$photo2 <- renderImage({
    list(
      src = "2.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  output$photo3 <- renderImage({
    list(
      src = "3.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  
  
  output$photo4 <- renderImage({
    list(
      src = "4.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  output$photo5 <- renderImage({
    list(
      src = "5.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  output$photo6 <- renderImage({
    list(
      src = "6.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  
  output$photo7 <- renderImage({
    list(
      src = "7.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
   
  
  
  
  
  
  output$beta <- renderText({
    fildata <- data2 %>% filter(stock_id == input$stock_id & time_id == input$time_id)
    b <- fildata$beta
  })
  
  output$spread <- renderText({
    fildata <- data2 %>% filter(stock_id == input$stock_id & time_id == input$time_id)
    b <- fildata$spread
  })
 
  output$dom <- renderText({
    fildata <- data2 %>% filter(stock_id == input$stock_id & time_id == input$time_id)
    b <- fildata$dom
  })
  
  output$target <- renderText({
    fildata <- data1 %>% filter(stock_id == input$s & time_id == input$t)
    b <- fildata$target_realized_volatility
  })
  
  output$predict <- renderText({
    fildata <- data1 %>% filter(stock_id == input$s & time_id == input$t)
    b <- fildata$target_realized_volatility
  })
  
  output$rmse <- renderText({
    fildata <- data1 %>% filter(stock_id == input$s & time_id == input$t)
    target_data <- fildata$target_realized_volatility
    predict_data <- fildata$predicted_volatility
    rm <- sqrt(mean((target_data - predict_data )^2))
  })
  
  
  
output$slect <- renderUI({
  fildata <- data3 %>% filter(stock_id == input$ss & time_id == input$tt)
  
  if (input$modl == "LGBM"){
    checkboxGroupInput("a",label = h3("Select model to show"),
                       choices = list("Normal" = fildata$lgbm.norm,"Simp" =fildata$lgbm.simp, "EXT"=fildata$lgbm.ext))
  }else if(input$modl == "POLY"){
    checkboxGroupInput("a",label = h3("Select model to show"),
                       choices = list("Rregration" = fildata$poly.reg))
    
  }else if (input$modl == "ENSEBLES") {
    checkboxGroupInput("a",label = h3("Select model to show"),
                       choices = list("Average" = fildata$ensemble.average,"ensemble.weighted" = fildata$ensemble.weighted))
    
  }
  
 
  fluidRow(column(3, verbatimTextOutput("value")))
  
})


output$value <- renderPrint({ input$a })
  
  
  
  
  
  
  output$plot <- renderPlot({
    start_time <- input$start
    end_time <- input$end
    #selet_x <- input$range
    m <- data1 %>% filter(stock_id == input$stock1) 
    a <- m[m$time_id >= start_time & m$time_id <= end_time,]
    
    
    #a <- subset(m, start_time > end_time) 
    #plot(a$time_id, a$predicted_volatility)
    #subset_time <- subset(m, time_id %in% selet_x)
    p1 = ggplot(a,aes(time_id,predicted_volatility))+geom_line(color= "#FF0000") + xlab("Time") + ylab("Predicted volatility") + ggtitle("Predicted volatility")
    p2 = ggplot(a,aes(time_id,target_realized_volatility))+geom_line(color = "#0000FF") + xlab("Time") + ylab("Target realized volatility") + ggtitle("Target realized volatility")
    grid.arrange(p1, p2)
  })
  
  
  
  
  
}



             
             
  


# Run the app ----
shinyApp(ui = ui, server = server)


