library(extrafont)
library(ggplot2)
library(leaflet)
library(rgdal)
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
measure_descr <-
  read_csv(paste(address, "project_metadata.csv", sep = ""))

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
                selectize = ,
                width = "180px"
              )
            ),
            
            #Economic data select (and help button)
            inputPanel(verticalLayout(
              selectInput(
                inputId = "econ_select",
                "Economic Indicator",
                choices = econ_selection,
                multiple = FALSE,
                selectize = T,
                width = "180px"
              ),
              actionButton(
                inputId = "help_econ",
                label = "What does this mean?",
                style = 'padding:4px; font-size:80%'
              )
            )),
            
            #Inequality data select (and help button)
            inputPanel(verticalLayout(
              selectInput(
                inputId = "inq_select",
                "Inequality Indicator",
                choices = inq_selection,
                multiple = FALSE,
                selectize = T,
                width = "180px"
              ),
              actionButton(
                inputId = "help_inq",
                label = "What does this mean?",
                style = 'padding:4px; font-size:80%'
              )
            )),
            
            #Output panels for help buttons
            conditionalPanel(condition = "input.help_econ%2 == 1 || input.help_inq%2 == 1", verticalLayout(
              textOutput("help_title", container = h4),
              textOutput("help_display")
            )),
            width = 4
          ),
          
          
          #view for country/data analysis
          mainPanel(
            #Javascript code acquried from StackOverflow; credit given to original author
            #adapted for use in this program by AS
            tags$head(
              tags$style(
                '#econ_tooltip {
                   position: absolute;
                   width: 200px;
                   z-index: 100;
                   padding: 0;
                 }
                 #inq_tooltip {
                   position: absolute;
                   width: 120px;
                   z-index: 100;
                   padding: 0;
                 }'
              )
            ),
            tags$script(
              '$(document).ready(function() {
                 // economic plot
                  $("#country_econ_plot").mousemove(function(e) {
                  // ID of uiOutput
                  $("#econ_tooltip").css({
                    top: (e.offsetY - 45) + "px",             
                    left: (e.offsetX + 20) + "px"
                  });
                  $("#econ_tooltip").show();
                });
                
                //inequality plot
                $("#country_inq_plot").mousemove(function(e) {
                  // ID of uiOutput
                  $("#inq_tooltip").css({
                    top: (e.offsetY + 355) + "px",             
                    left: (e.offsetX + 20) + "px"
                  });
                  $("#inq_tooltip").show();
                });
            });'
            ), 
            uiOutput("econ_tooltip"),
            uiOutput("inq_tooltip"),
            plotOutput(outputId = "country_econ_plot", hover = "econ_hover", hoverDelay = 0),
            plotOutput(outputId = "country_inq_plot", hover = "inq_hover", hoverDelay = 0),
            verbatimTextOutput(outputId = "correlation_stats", placeholder = T)
          )
        )
      ),
      tabPanel("New Zealand's Impact",
               verticalLayout(
                 headerPanel(title = "New Zealand's Trade and Inequality"),
                 textOutput(outputId = "nz_descr"),
                 leafletOutput(outputId = "nz_map")
               ))
    ), 
    title = "Trade Balance and Inequality: An Analysis",
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
              "(million USD)",
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
          geom_point(shape = 17, color = "#0066CC", size = 5, show.legend = F) +
          geom_line(alpha = 0.5, color = "black", size = 0.5) +
          scale_x_continuous(breaks = c(2008:2017), limits = c(2008,2017)) + 
          labs(
            x = "Year",
            y = input$inq_select
          )        
      })
    
    #--------Render tooltips for graph hovering---------
    
    #Render Economic tooltip
    output$econ_tooltip <- renderUI({
      #First formulate the data to plot
      plot_econdata <-
        econdata[, c("Country", gsub(" ", "_", input$econ_select))]
      plot_econdata <-
        plot_econdata[plot_econdata$Country == input$country_select, 2]
      plot_econ_time <-
        econdata[econdata$Country == input$country_select, "Year"]
      
      #represents the final formatting of the selected data
      econplot <- cbind(plot_econ_time, plot_econdata)
      
      #Then do tooltip stuff
      hover <- input$econ_hover
      y <- nearPoints(econplot, input$econ_hover, xvar = "Year", yvar = gsub(" ", "_", input$econ_select))[gsub(" ", "_", input$econ_select)]
      req(nrow(y) != 0)
      verbatimTextOutput("econvals")
    })
    output$econvals <- renderText({
      #First formulate the data to plot
      plot_econdata <-
        econdata[, c("Country", gsub(" ", "_", input$econ_select))]
      plot_econdata <-
        plot_econdata[plot_econdata$Country == input$country_select, 2]
      plot_econ_time <-
        econdata[econdata$Country == input$country_select, "Year"]
      
      #represents the final formatting of the selected data
      econplot <- cbind(plot_econ_time, plot_econdata)
      
      #Then do tooltip stuff
      hover <- input$econ_hover 
      x <- nearPoints(econplot, input$econ_hover, xvar = "Year", yvar = gsub(" ", "_", input$econ_select))[1]
      y <- nearPoints(econplot, input$econ_hover, xvar = "Year", yvar = gsub(" ", "_", input$econ_select))[gsub(" ", "_", input$econ_select)]
      req(nrow(y) != 0)
      
      to_output <- paste("In ", x, ": ", y, sep = "")
      to_output
    })  
    
    #Render Inequality Tooltip
    output$inq_tooltip <- renderUI({
      #First formulate the data to plot
      plot_inqdata <-
        inqdata[, c("Country", gsub(" ", "_", input$inq_select))]
      plot_inqdata <-
        plot_inqdata[plot_inqdata$Country == input$country_select, 2]
      plot_inq_time <-
        inqdata[inqdata$Country == input$country_select, "Year"]
      
      #represents the final formatting of the selected data
      inqplot <- cbind(plot_inq_time, plot_inqdata)
      
      #Then do tooltip stuff
      hover <- input$inq_hover
      y <- nearPoints(inqplot, input$inq_hover, xvar = "Year", yvar = gsub(" ", "_", input$inq_select))[gsub(" ", "_", input$inq_select)]
      req(nrow(y) != 0)
      verbatimTextOutput("inqvals")
    })
    output$inqvals <- renderText({
      #First formulate the data to plot
      plot_inqdata <-
        inqdata[, c("Country", gsub(" ", "_", input$inq_select))]
      plot_inqdata <-
        plot_inqdata[plot_inqdata$Country == input$country_select, 2]
      plot_inq_time <-
        inqdata[inqdata$Country == input$country_select, "Year"]
      
      #represents the final formatting of the selected data
      inqplot <- cbind(plot_inq_time, plot_inqdata)
      
      #Then do tooltip stuff
      hover <- input$inq_hover
      x <- nearPoints(inqplot, input$inq_hover, xvar = "Year", yvar = gsub(" ", "_", input$inq_select))[1]
      y <- nearPoints(inqplot, input$inq_hover, xvar = "Year", yvar = gsub(" ", "_", input$inq_select))[gsub(" ", "_", input$inq_select)]
      req(nrow(y) != 0)
      
      to_output <- paste("In ", x, ": ", y, sep = "")
      to_output
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
    
    #render NZ-centric panel, including text & leaflet map
    output$nz_descr<- renderText({
      "description"
    })
    output$nz_map <- renderLeaflet({
      #setup the map bounds and groups
      m <- geojsonio::geojson_read("C:/Users/Asher (GOD)/Desktop/VUW/2019_tri_3/INFO281/project_material/mapdata/countries.geo.json", what = "sp")
      ydatam <- m[m$id %in% inqdata$Country_code[inqdata$Country_code != "AGO" & inqdata$Country_code != "GNB"], ]
      ndatam <- m[m$id %in% inqdata$Country_code[inqdata$Country_code == "AGO" | inqdata$Country_code == "GNB"], ]
      nzdatam <- m[m$id == "NZL", ]
      
      #Begin rendering map object
      map <- leaflet(options =leafletOptions(minZoom = 1, maxZoom = 5)) %>%
        addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
        
        #render country borders (with data); due to nature of data set, Comoros, Sao Tome and Principe, and Tuvalu are not rendered
        addPolygons(
          data = ydatam,
          fillColor = "#FFF333",
          weight = 1,
          popup = paste0("<h4><strong>", ydatam$name, "</strong></h4>"),
          opacity = 1,
          color = 'black',
          dashArray = '0',
          fillOpacity = 0.5,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) %>%
        #render country borders (w/out data); due to nature of data set, Comoros, Sao Tome and Principe, and Tuvalu are not rendered
        addPolygons(
          data = ndatam,
          fillColor = "#A0A0A0",
          weight = 1,
          popup = paste0("<h4><strong>", ndatam$name, "</strong></h4>"),
          opacity = 1,
          color = 'black',
          dashArray = '0',
          fillOpacity = 0.5,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) %>%
        #render NZ borders for the fun of it
        addPolygons(
          data = nzdatam,
          fillColor = "#66B2FF",
          weight = 1,
          popup = paste0("<h4><strong>New Zealand</strong></h4>"),
          opacity = 1,
          color = 'black',
          dashArray = '0',
          fillOpacity = 0.5,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        )
      
      #return finished map object
      map
    })
    
    
  }



# --------RUN APPLICATION--------
shinyApp(ui = ui, server = server)