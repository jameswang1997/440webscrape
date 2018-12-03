library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(forecast)
library(knitr)
library(car)
setwd('/Users/JamesWang1/Documents/STA440/440webscrape/')

df.join = read_csv('df.join.csv')
df.star1 = df.join %>% dplyr::select(name, gender, d1,d2, d3, cup, height, weight, status, video_view1)
star_var = names(df.star1)[-1]

star_model_var = c('d1', 'd2', 'd3', 'cup' , 'height' , 'weight')

# ggplot(data = df.star1,aes(x = height, y= weight)) +
#   geom_point()

# Define UI
ui <- fluidPage(
  theme = shinytheme('cyborg'),
  
  titlePanel('PornHub Data Analysis'),
  
  tabsetPanel(
    tabPanel("Stars", fluid = TRUE, 
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = 'var_bio_x', label = strong("Porn Star Var X"),
                      choice = star_var,
                      selected = 'height'
                      
          ),
          selectInput(inputId = 'var_bio_y', label = strong("Porn Star Var Y"),
                      choice = c("None",star_var),
                      selected = 'None'
                      
          ), 
          br(),
          actionButton('modelStarButton', "Build Model")
          
          
        ), 
        # OutPUT: scatterplot
        mainPanel(
          wellPanel(
            # HTML('<h4> Visuals </h4>'),
            
            plotOutput(outputId = 'scatterplot', height = '300px')
            
            
          )
        )
      ),
      # second sidebar
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = 'avVar', label = strong('Prector'),
                      choice = star_model_var,
                      selected = 'weight'
                      )
        ),
        
        mainPanel(
          wellPanel(
            plotOutput(outputId = 'avPlot', height = '300px')
          )
        )
        
      )
      
      
      
    ), 
    # ===================================
    # star tab ends
    # ===================================
    
    tabPanel("Video", fluid = T,
             sidebarLayout(
               sidebarPanel(

               ),
              
               mainPanel()
             )



             )

  )
  
  
 
  
)


server <-function(input, output){
  
  # model
  
  model_star <- eventReactive( input$modelStarButton, {
    lm(log(video_view1) ~d1 + d2 + d3 + cup + height + weight, data = df.star1)
  } )


  # create plotOutput
  output$scatterplot <- renderPlot( {
    if (input$var_bio_y == 'None') {
      base = ggplot(data = df.star1,aes_string(x = input$var_bio_x))
      
      if (is.numeric(df.star1[[input$var_bio_x]])) {
        base + geom_histogram()
      } else {
        base + geom_bar(aes_string(fill= input$var_bio_x))
      }
    } else { # if y is used
      if (is.numeric(df.star1[[input$var_bio_x]]) & is.numeric(df.star1[[input$var_bio_y]])) { # both are numeric

          ggplot(data = df.star1,aes_string(x = input$var_bio_x, y= input$var_bio_y)) +
              geom_point()  }
      
      
    }  
   
    
     
  }
   
 )
  
  output$avPlot <- renderPlot({
    req(input$modelStarButton)
    avPlot(model_star(), input$avVar)
  })
  
  
  
}

shinyApp(ui =ui, server = server)


