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
  navbarPage(
    "My Project",
    #Country Analysis tab
    tabPanel("Country Analysis", fluidPage(
      #Viewing and manipulating country analysis
      sidebarLayout(
        #Data selection for country/data analysis
        sidebarPanel(
          #Country select (and help button)
          inputPanel(
            selectInput(
              inputId = "country_select",
              "Country",
              choices = country_selection,
              multiple = FALSE,
              width = 250
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
              width = 250
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
              width = 250
            ),
            actionButton(
              inputId = "inq_help",
              label = "",
              icon = helpicon
            )
          )
        ),
        
        #Do analysis 
        
        #view for country/data analysis
        mainPanel(verticalLayout(
          plotOutput(outputId = "country_econ_plot"),
          plotOutput(outputId = "country_inq_plot"),
          width = 8
        ))
      )
    )),
    tabPanel("New Zealand Analysis")
  )




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
  }




# --------RUN APPLICATION--------
shinyApp(ui = ui, server = server)