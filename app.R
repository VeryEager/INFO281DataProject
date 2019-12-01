library(shiny)

# --------PRELIMINARY OPERATIONS--------
#icon used in "Help" buttons
helpicon <-
  icon("question-circle", class = NULL, lib = "font-awesome")

#available options for selecting country-by-country data
country_selection <- unique(econdata$Country)

econ_selection <- colnames(econdata)
econ_selection <- econ_selection[-(1:5)]
econ_selection <- gsub("_", " ", econ_selection)

inq_selection <- colnames(inqdata)
inq_selection <- inq_selection[-(1:3)]
inq_selection <- gsub("_", " ", inq_selection)




# --------FORMAT UI--------
ui <-
  fluidPage(tabsetPanel(
    #Country Analysis tab
    tabPanel("Country Analysis", fluidPage(
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
          verbatimTextOutput(outputId = "correlation_stats", placeholder = T),
        ),
        fluid = T
      )
    )),
    tabPanel("New Zealand Analysis")
  ))




# --------FORMAT SERVER--------
server <-
  function(input, output) {
    #render economic plot
    output$country_econ_plot <-
      renderPlot({
        plot(econdata$Year,
             econdata$Year,
             xlab = "Year",
             ylab = econdata$Units[1]) #TODO: econdata$Year needs to be changed to reflect appropriate row/col values
      })
    
    #render inequality plot
    output$country_inq_plot <-
      renderPlot({
        plot(inqdata$Year,
             inqdata$Year,
             xlab = "Year",
             ylab = "Index") #TODO: inqdata$Year and index need to be changed to reflect appropriate data
      })
    
    #render statistical summary of country data
    output$correlation_stats <-
      renderText({
        "PLACEHOLDER; STATISTICAL SUMMARY WILL BE DISPLAYED HERE."
      })
  }




# --------RUN APPLICATION--------
shinyApp(ui = ui, server = server)