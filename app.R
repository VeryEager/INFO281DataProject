library(shiny)

# --------PRELIMINARY OPERATIONS--------
#icon used in "Help" buttons
helpicon <-
  icon("question-circle", class = NULL, lib = "font-awesome")

#available options for selecting country-by-country data
country_selection <- unique(econdata$Country)
econ_selection <- colnames(econdata)
inq_selection <- colnames(inqdata)


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
          
          #Economic data select (and help)
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
          
          #Inequality data select (and help)
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
        #view for country/data analysis
        mainPanel(plotOutput(outputId = "country_plot"),
                  width = 8)
      )
    )),
    tabPanel("New Zealand Analysis")
  )


# --------FORMAT SERVER--------
server <-
  function(input, output) {
    output$country_plot <- renderPlot({
      plot(mtcars$wt, mtcars$mpg)
    })
  }


# --------RUN APPLICATION--------
shinyApp(ui = ui, server = server)