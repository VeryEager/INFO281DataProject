library(extrafont)
library(ggplot2)
library(shiny)
library(shinythemes)
library(tidyverse)

#---------PRELIMINARY OPERATIONS FOR DISPLAY---------
options(scipen = 999)
extrafont::loadfonts(device = "win")

# --------PRELIMINARY OPERATIONS--------
#icon used in "Help" buttons (since removed)
helpicon <-
  icon("question-circle", class = NULL, lib = "font-awesome")

#dataframe used in "help" button presses
measure_descr <- read_csv(paste(address, "project_metadata.csv", sep = ""))

#available options for selecting country-by-country data
country_selection <- unique(econdata$Country)

econ_selection <- colnames(econdata)
econ_selection <- econ_selection[-(1:4)]
econ_selection <- gsub("_", " ", econ_selection)
econ_selection <- sort(econ_selection)

inq_selection <- colnames(inqdata)
inq_selection <- inq_selection[-(1:3)]
inq_selection <- gsub("_", " ", inq_selection)



# --------FORMAT UI--------
ui <-
  fluidPage(
    tabsetPanel(
      #Country Analysis tab
      tabPanel("Overview"),
      tabPanel(
        "Country Exploration",
        #Viewing and manipulating country analysis
        sidebarLayout(
          #Data selection for country/data analysis
          sidebarPanel(
            #Country select
            h4("Data Selection"),
            
            inputPanel(
              selectInput(
                inputId = "country_select",
                "Country",
                choices = country_selection,
                multiple = FALSE,
                selectize = T
              )
            ),
            
            #Economic data select (and help button)
            inputPanel(
              selectInput(
                inputId = "econ_select",
                "Economic Indicator",
                choices = econ_selection,
                multiple = FALSE,
                selectize = T
              ),
              actionButton(
                inputId = "help_econ",
                label = "What does this mean?",
              )
            ),
            
            #Inequality data select (and help button)
            inputPanel(
              selectInput(
                inputId = "inq_select",
                "Inequality Indicator",
                choices = inq_selection,
                multiple = FALSE,
                selectize = T
              ),
              actionButton(
                inputId = "help_inq",
                label = "What does this mean?",
              )
            ),
            
            #Output panels for help buttons
            inputPanel(textOutput("help_title", container = h4), textOutput("help_display", container = h5)),
            
            width = 3
          ),
          
          #Do analysis
          
          #view for country/data analysis
          mainPanel(
            plotOutput(outputId = "country_econ_plot"),
            plotOutput(outputId = "country_inq_plot"),
            verbatimTextOutput(outputId = "correlation_stats", placeholder = T)
          ),
          fluid = T
        )
      ),
      tabPanel("New Zealand's Impact")
    ),
    
    title = "Trade Balance and Inequality; an Analysis",
    theme = shinytheme("sandstone")
  )




# --------FORMAT SERVER--------
server <-
  function(input, output) {
    #render economic plot
    output$country_econ_plot <-
      renderPlot({
        #First formulate the data to plot
        plot_econdata <-
          econdata[, c("Country", gsub(" ", "_", input$econ_select))]
        plot_econdata <-
          plot_econdata[plot_econdata$Country == input$country_select, 2]
        plot_econ_time <-
          econdata[econdata$Country == input$country_select, "Year"]
        
        #represents the final formatting of the selected data
        econplot <- cbind(plot_econ_time, plot_econdata)
        
        #Then plot an interactive scatterplot using ggplot
        ggplot(data = econplot, aes(x = econplot[[1]], y = econplot[[2]], color = econplot[[2]]), group = 1) +
          theme(text = element_text(family = "Arial", size = 16)) +
          geom_point(shape = 15, size = 5, show.legend = F) +
          geom_line(alpha = 0.5, color = "black", size = 0.5) +
          scale_x_continuous(breaks = econplot[[1]]) + 
          labs(
            x = "Year",
            y = paste(
              input$econ_select,
              "(millions USD)",
              sep = " "
            )
          ) +
          scale_color_gradient2(midpoint = 0, low = "#CC0000", mid = "#FFCC33", high = "#33FF00")
      })
    
    #render inequality plot
    output$country_inq_plot <-
      renderPlot({
        #First formulate the data to plot
        
        plot_inqdata <-
          inqdata[, c("Country", gsub(" ", "_", input$inq_select))]
        plot_inqdata <-
          plot_inqdata[plot_inqdata$Country == input$country_select, 2]
        plot_inq_time <-
          inqdata[inqdata$Country == input$country_select, "Year"]
        
        #represents the final formatting of the selected data
        inqplot <- cbind(plot_inq_time, plot_inqdata)
        
        #Then plot an interactive scatterplot using ggplot
        ggplot(data = inqplot, aes(x = inqplot[[1]], y = inqplot[[2]]), group = 1) +
          theme(text = element_text(family = "Arial", size = 16)) +
          geom_point(shape = 15, color = "#0066CC", size = 5, show.legend = F) +
          geom_line(alpha = 0.5, color = "black", size = 0.5) +
          scale_x_continuous(breaks = inqplot[[1]]) + 
          labs(
            x = "Year",
            y = input$inq_select
          )        
      })
    
    #render statistical summary of country data
    output$correlation_stats <-
      renderText({
        "PLACEHOLDER; STATISTICAL SUMMARY WILL BE DISPLAYED HERE."
      })
    
    #render descriptive text when help buttons are pressed
    help_title_text <- reactiveValues(text = "")
    help_display_text <- reactiveValues(text = "")
    
    observeEvent(input$help_inq, {
      help_title_text$text <- input$inq_select
      help_display_text$text <- unlist(measure_descr[measure_descr$Measure == input$inq_select, 2])
    })
    
    observeEvent(input$help_econ, {
      help_title_text$text <- input$econ_select
      help_display_text$text <- unlist(measure_descr[measure_descr$Measure == input$econ_select, 2])
    })
    
    output$help_title <- renderText({
      help_title_text$text
    })
    
    output$help_display <- renderText({
      help_display_text$text
    })
  }



# --------RUN APPLICATION--------
shinyApp(ui = ui, server = server)