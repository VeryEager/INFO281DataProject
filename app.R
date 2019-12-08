library(shiny)
library(shinythemes)

options(scipen = 999)

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
              ),
              actionButton(
                inputId = "country_help",
                label = "",
                icon = helpicon
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
        plot_economy <-
          econdata[, c("Country", gsub(" ", "_", input$econ_select))]
        plot_economy <-
          plot_economy[plot_economy$Country == input$country_select, 2]
        plot_econ_time <-
          econdata[econdata$Country == input$country_select, "Year"]
        
        plot.default(
          as.numeric(unlist(plot_econ_time)),
          as.numeric(unlist(plot_economy)),
          type = "b",
          xlab = "Year",
          ylab = paste(econdata$Units[1], " (millions)", sep = " "),
          col = "#75AADB"
        )
        
      })
    
    #render inequality plot
    output$country_inq_plot <-
      renderPlot({
        plot_inequality <-
          inqdata[, c("Country", gsub(" ", "_", input$inq_select))]
        plot_inequality <-
          plot_inequality[plot_inequality$Country == input$country_select, 2]
        plot_inq_time <-
          inqdata[inqdata$Country == input$country_select, "Year"]
        
        plot.default(
          as.numeric(unlist(plot_inq_time)),
          as.numeric(unlist(plot_inequality)),
          type = "b",
          xlab = "Year",
          ylab = input$inq_select,
          col = "#75AADB"
        )
      })
    
    #render statistical summary of country data
    output$correlation_stats <-
      renderText({
        "PLACEHOLDER; STATISTICAL SUMMARY WILL BE DISPLAYED HERE."
      })
  }



# --------RUN APPLICATION--------
shinyApp(ui = ui, server = server)