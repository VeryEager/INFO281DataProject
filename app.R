library(shiny)
library(shinythemes)
library(extrafont)
library(ggplot2)
library(plotly)

#---------PRELIMINARY OPERATIONS FOR DISPLAY---------
options(scipen = 999)
extrafont::loadfonts(device = "win")

# --------PRELIMINARY OPERATIONS--------
#icon used in "Help" buttons
helpicon <-
  icon("question-circle", class = NULL, lib = "font-awesome")

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
            #Country select (and help button)
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
                inputId = "econ_help",
                label = "",
                icon = helpicon
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
                inputId = "inq_help",
                label = "",
                icon = helpicon
              )
            ),
            
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
          scale_color_gradient2(midpoint = 0, low = "red", mid = "#FFCC33", high = "green")
      })
    
    #render inequality plot
    output$country_inq_plot <-
      renderPlot({
        #First formulate the data to plot
        
        plot_inequality <-
          inqdata[, c("Country", gsub(" ", "_", input$inq_select))]
        plot_inequality <-
          plot_inequality[plot_inequality$Country == input$country_select, 2]
        plot_inq_time <-
          inqdata[inqdata$Country == input$country_select, "Year"]
        
        
        #Then plot an interactive scatterplot using ggplot
        
      })
    
    #render statistical summary of country data
    output$correlation_stats <-
      renderText({
        "PLACEHOLDER; STATISTICAL SUMMARY WILL BE DISPLAYED HERE."
      })
  }



# --------RUN APPLICATION--------
shinyApp(ui = ui, server = server)