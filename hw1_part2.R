## Part 2: hw1_part2
# Changing the UI

# Part 1: hw1_part1
# Simple Data Summary and Visualization

# load the packages 
library(shiny)
library(ggplot2)

# load the data sets and do some data cleaning
require(palmerpenguins)
penguins <- na.omit(transform(penguins, year = factor(year)))
cols <- c(2,8:11)
mtcars[cols] <- lapply(mtcars[cols], factor)
str(mtcars)


ui <- fluidPage(
  # application title
  titlePanel("Simple Visualization"), 
  
  sidebarLayout(
    sidebarPanel(
      # choose a data set 
      radioButtons(inputId = "data",
                  label = "select a dataset",
                  choices = c("mtcars", "penguins")),
      # select a variable from the selected data set
      selectInput(inputId = "variable",
                  label = "choose a variable",
                  choices = NULL),
      
      # UI created in the server slider is now conditional on the chosen variable
      uiOutput("slider"),
      
      # UI created in the server drop-down is now conditional on the chosen variable
      uiOutput("group_option")
    ),
    # show a plot of the generated distribution
    mainPanel(
      plotOutput("histogram")
    )
  )
)



server <- function(input, output) {
  
  # create a data loading reactive function to call in data
  dat <- reactive({
    if(input$data == "mtcars"){dat <- mtcars}else{dat <- penguins}
    return(dat)
  })
  observeEvent(input$data, {
    updateSelectInput(inputId = "variable",
                      choices = names(dat()))
  })
  
  # create a UI object that pops up when a continuous variable is selected 
  output$group_option <- renderUI({
    get_type <- function(x){
      # if the variable is not numeric return discrete
      if(is.numeric(x) == FALSE){type <- "discrete"}
      # if the variable is numeric but has less than 5 unique values return discrete
      else if(length(unique(x)) < 5){type <- "discrete"}
      # everything else marks continuous
      else{type <- "continuous"}
      
      return(type)
    }
    
    # get the type of the selected variable，input$variable if the ID that denotes which variable is selected
    type <- get_type(dat()[[input$variable]])
    # if the type is continuous add a drop-down to the UI to pick a grouping variable of.
    # it will need an ID lets call it group_variable
    if(type == "continuous"){
      radioButtons(inputId = "group_variable",
                  label = "choose a varibale to color the plot",
                  choices = " ")
    } else{NULL} # no new UI object if variable is not continuous
  })
  
  # second step is to update options 
  
  observeEvent(input$variable, {
    
    get_type <- function(x){
      # if the variable is not numeric return discrete 
      if(is.numeric(x) == FALSE){type <- 'discrete'} 
      
      # if the variable is numeric but has less than 5 unique values return discrete
      else if(length(unique(x))<5){type <- 'discrete'}
      
      # everything else mark continuous
      else{type <- 'continuous'}
      
      
      return(type)
    }
    
    # define a discrete vector for group option
    types <- unlist(lapply(dat(), get_type)) # get vector of data types for all columns in the selected df 
    discrete_vars <- names(dat()[, types == "discrete"]) # get the names of each discrete variable
    
    
    updateRadioButtons(inputId = "group_variable", 
                      choices = discrete_vars)
  })
  
  
  # create a slider to change the number of bins
  output$slider <- renderUI({
    get_type <- function(x){
      # if the variable is not numeric return discrete
      if(is.numeric(x) == FALSE){type <- "discrete"}
      # if the variable is numeric but has less than 5 unique values return discrete
      else if(length(unique(x)) < 5){type <- "discrete"}
      # everything else marks continuous
      else{type <- "continuous"}
      
      return(type)
    }
    
    # get the type of the selected variable
    type <- get_type(dat()[[input$variable]])
    # if the type is continuous add a slider to the UI 
    if(type == "continuous"){
      numericInput(inputId = "bins",
                   label = "Number of bins:",
                   value = 20,
                   min = 5,
                   max = 50)
    }else{NULL} # if not don't return any UI objects 
  })
  
  
  # create the histograms
  output$histogram <- renderPlot({
    
    # normal R function to get variable type note it is INSIDE of a reactive function  
    get_type <- function(x){
      # if the variable is not numeric return discrete
      if(is.numeric(x) == FALSE){type <- "discrete"}
      # if the variable is numeric but has less than 5 unique values return discrete
      else if(length(unique(x)) < 5){type <- 'discrete'}
      # everything else mark continuous
      else{type <- 'continuous'}
      # output the type
      return(type)
    }
    
    # get the type of a selected variable
    type <- get_type(dat()[[input$variable]])
    if(type == "continuous"){
      # if the variable selected is continuous make this plot:
      ggplot(data = dat(), aes_string(input$variable, fill = input$group_variable)) + 
        geom_histogram(bins = input$bins, alpha = 0.7, position = "identity") +
        theme_bw()
    }else if(type == "discrete"){
      # if the selected variable is discrete make this plot:
      ggplot(data = dat(), aes_string(input$variable)) +
        geom_histogram(stat = "count") +
        theme_bw()
    }
  }) 
  
  
  
}


shinyApp(ui = ui, server = server)













