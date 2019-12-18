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
      tabPanel("Overview",
               verticalLayout(
                 sidebarLayout(
                   sidebarPanel(
                     h4("Data Selection"),
                     uiOutput(outputId = "ov_sidebar_descr"),
                     
                     #Economic indicator selection for the overview screen
                     inputPanel(
                       selectInput(
                         inputId = "ov_econ_select",
                         "Economic Indicator",
                         choices = econ_selection[grepl("net", econ_selection, ignore.case = T) |
                                                    grepl("balance", econ_selection, ignore.case = T)],
                         multiple = FALSE,
                         selectize = T,
                         width = "180px"
                       )
                     ),
                     
                     #Inequality indicator selection for the overview screen
                     inputPanel(
                       selectInput(
                         inputId = "ov_inq_select",
                         "Inequality Indicator",
                         choices = inq_selection,
                         multiple = FALSE,
                         selectize = T,
                         width = "180px"
                       )
                     ),
                     
                     #Data on the correlation between the selection
                     uiOutput(outputId = "ov_corr_descr"),
                     width = 4
                   ),
                   mainPanel(
                     #GGPlot which displays the correlation between the data, with an additional tooltip
                     tags$head(
                       tags$style(
                         '#corr_tooltip {
                            position: absolute;
                             width: 200px;
                            z-index: 100;
                             padding: 0;
                           }'
                       )
                     ),
                     tags$script(
                       '$(document).ready(function() {
                         // correlation plot
                         $("#ov_plot").mousemove(function(e) {
                         // ID of uiOutput
                         $("#corr_tooltip").css({
                            top: (e.offsetY - 45) + "px",
                            left: (e.offsetX + 20) + "px"
                            });
                          $("#corr_tooltip").show();
                     });'),
                     plotOutput(outputId = "ov_plot", hover = "corr_hover", hoverDelay = 0)
                   )
                 ),
                 hr(),
                 
                 sidebarLayout(
                   sidebarPanel(
                     #Summary of the correlations; describe general patterns
                     uiOutput(outputId = "ov_summary"),
                     width = 5),
                   mainPanel(
                     #Buttons for choosing variable displayed on the table
                     inputPanel(radioButtons(
                       inputId = "table_val",
                       "Selected Statistic: ",
                       choices = c("P-value", "R-value")
                     )), 
                     #Table displaying correlation statistic for all combos.
                     DT::dataTableOutput(outputId = "ov_table"),
                     width = 7
                     
                   )
                 )
               )),
      tabPanel(
        "Country Exploration",
        #Viewing and manipulating country analysis
        sidebarLayout(#Data selection for country/data analysis
          sidebarPanel(
            verticalLayout(
              sidebarPanel(
                #Country select
                h4("Data Selection"),
                
                inputPanel(
                  selectInput(
                    inputId = "country_select",
                    "Country",
                    choices = country_selection,
                    multiple = FALSE,
                    selectize = T,
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
                width = 12
              ),
              
              #Statistics box on side of page
              sidebarPanel(verticalLayout(
                h3("Statistics on Selected Data"),
                flowLayout(
                  uiOutput(outputId = "econ_stats"),
                  uiOutput(outputId = "inq_stats")
                )
              ), width = 12)
            ), width = 4
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
            })
'
            ),
            uiOutput("econ_tooltip"),
            uiOutput("inq_tooltip"),
            plotOutput(outputId = "country_econ_plot", hover = "econ_hover", hoverDelay = 0),
            plotOutput(outputId = "country_inq_plot", hover = "inq_hover", hoverDelay = 0)
          )
        )
      ),
      tabPanel("New Zealand's Impact",
               verticalLayout(
                 sidebarPanel(
                   uiOutput(outputId = "nz_descr"), width = 12),
                 leafletOutput(outputId = "nz_map")
               ))
    ),
    title = "Trade Balance and Inequality:An Analysis",
    theme = shinytheme("sandstone")
  )



# --------FORMAT SERVER--------
server <-
  function(input, output) {
    
    #render economic plot
    output$country_econ_plot <-
      renderPlot({
        #Formulate data to be plotted
        econplot <- formulate.econ.data(input)
        
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
        #Formulate data to be plotted
        inqplot <- formulate.inq.data(input)
        
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
      #Formulate data to be plotted
      econplot <- formulate.econ.data(input)
      
      #Then do tooltip stuff
      hover <- input$econ_hover
      y <- nearPoints(econplot, input$econ_hover, xvar = "Year", yvar = gsub(" ", "_", input$econ_select))[gsub(" ", "_", input$econ_select)]
      req(nrow(y) != 0)
      verbatimTextOutput("econvals")
    })
    output$econvals <- renderText({
      #Formulate data to be plotted
      econplot <- formulate.econ.data(input)
      
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
      #Formulate data to be plotted
      inqplot <- formulate.inq.data(input)
      
      #Then do tooltip stuff
      hover <- input$inq_hover
      y <- nearPoints(inqplot, input$inq_hover, xvar = "Year", yvar = gsub(" ", "_", input$inq_select))[gsub(" ", "_", input$inq_select)]
      req(nrow(y) != 0)
      verbatimTextOutput("inqvals")
    })
    
    #Render Inequality Tooltip text
    output$inqvals <- renderText({
      #Formulate data to be plotted
      inqplot <- formulate.inq.data(input)
      
      #Then do tooltip stuff
      hover <- input$inq_hover
      x <- nearPoints(inqplot, input$inq_hover, xvar = "Year", yvar = gsub(" ", "_", input$inq_select))[1]
      y <- nearPoints(inqplot, input$inq_hover, xvar = "Year", yvar = gsub(" ", "_", input$inq_select))[gsub(" ", "_", input$inq_select)]
      req(nrow(y) != 0)
      
      to_output <- paste("In ", x, ": ", y, sep = "")
      to_output
    })  
    
    #--------Render Statistical Summary--------
    #render statistical summary of selected economic data
    output$econ_stats <-
      renderText({
        textdata <- formulate.econ.data(input)
        generate.stats(textdata, input, "econ")
      })
    
    #render statistical summary of selected inequality data
    output$inq_stats <-
      renderText({
        textdata <- formulate.inq.data(input)
        generate.stats(textdata, input, "inq")
      })
    
    #--------Render Help Text--------
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
    
    #--------Render NZ Page and Map--------
    #render NZ-centric panel, including text & leaflet map
    output$nz_descr <- renderText({
      "<h3>New Zealand's Trade and Inequality</h3>
      <div>The interactive map below displays information about developing countries' trade relationships with New Zealand. Some countries have
      been omitted due to a lack of reliable geographic or trade data. Countries marked <font color = \"#A0A0A0\">grey</font> do not have any 
      relevant trade data freely available. Countries marked <font color = \"#CCCC00\">yellow</font> provide some data about their trade 
      relationship with New Zealand. New Zealand has been marked <font color = \"#3a8de0\">blue</font>. All values are in thousand USD (2017), 
      unless otherwise specified.</div>"
    })
    output$nz_map <- renderLeaflet({
      #setup the map bounds and groups
      m <- geojsonio::geojson_read("./mapdata/countries.geo.json", what = "sp")
      ydatam <- m[m$id %in% inqdata$Country_code[inqdata$Country_code != "AGO" & inqdata$Country_code != "GNB"], ]
      ydatam <- ydatam[order(ydatam$id), ]
      ndatam <- m[m$id %in% inqdata$Country_code[inqdata$Country_code == "AGO" | inqdata$Country_code == "GNB"], ]
      
      nzdatam <- m[m$id == "NZL", ]
      
      #Begin rendering map object
      map <- leaflet(options =leafletOptions(minZoom = 3, maxZoom = 5)) %>%
        addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
        
        #render country borders (with data); due to nature of data set, Comoros, Sao Tome and Principe, and Tuvalu are not rendered
        addPolygons(
          data = ydatam,
          fillColor = generate.map.fill(ydatam$id, ydatam),
          weight = 1,
          #The popup requries the data be 'reversed' from NZ's perspective (ie, "Exports to *country*" becomes "Imports from NZ")
          popup = generate.map.text(ydatam$id, ydatam),
          opacity = 1,
          color = 'black', 
          dashArray = '0',
          fillOpacity = 0.5,
          highlight = highlightOptions(
            fillColor = generate.map.highlight(ydatam$id, ydatam), 
            weight = 2.5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) %>%
        #render country borders (w/out data); due to nature of data set, Comoros, Sao Tome and Principe, and Tuvalu are not rendered
        addPolygons(
          data = ndatam,
          fillColor = generate.map.fill(ndatam$id, ndatam),
          weight = 1,
          popup = generate.map.text(ndatam$id, ndatam),
          opacity = 1,
          color = 'black',
          dashArray = '0',
          fillOpacity = 0.5,
          highlight = highlightOptions(
            fillColor = generate.map.highlight(ndatam$id, ndatam),
            weight = 2.5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) %>%
        #render NZ borders for the fun of it
        addPolygons(
          data = nzdatam,
          fillColor = generate.map.fill("NZL", nzdata),
          weight = 1,
          popup = generate.map.text("NZL", nzdata),
          opacity = 1,
          color = 'black',
          dashArray = '0',
          fillOpacity = 0.5,
          highlight = highlightOptions(
            fillColor = generate.map.highlight("NZL", nzdata),
            weight = 2.5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        )
      
      #return finished map object
      map
    })
    
    
    
    
    #--------RENDER OVERVIEW PAGE--------
    #General text displays
    output$ov_sidebar_descr <- renderText({
      paste0(
        "<div>Select an economic indicator and an inequality indicator to see their relationship plotted and the correlation calculated.</div>"
      )
    })
    output$ov_corr_descr <- renderText({
      #Formulate data to calculate the correlation
      plot_corrdata <-
        aggrdata[, c(gsub(" ", "_", input$ov_inq_select),
                     gsub(" ", "_", input$ov_econ_select))]
      test <-
        psych::corr.test(plot_corrdata[c(1, 2)], method = "spearman")
      
      paste0(
        "<h4>Correlation Statistics</h4><div>P-value: <font color = \"blue\" face = \"helvetica\">",
        round(test$p[2], digits = 3),
        "</font><br/>R-value: <font color = \"blue\" face = \"helvetica\">",
        round(test$r[2], digits = 3),
        "</font></div>Correlation Strength: ", generate.corr.text(test$r[2])
      )
    })
    output$ov_summary <- renderText({
      paste0("<h3>Summary of the Correlations</h3><div>To the right is an interactive table which displays the Correlation Coeffeicients and 
             Probability Values for all pairings of economic and inequality indicators. All correlation tests were performed using the 
             Spearman method.</div><div>In broad terms, the correlations between the economic and inequality indicators are weak (using alpha = 
             <font color = \"blue\">0.05</font>). However, this is not the case with all relationships. Net Fuels and
             Mining Products is positively correlated with the Poverty Headcount Ratio. Travel Services is negatively correlated with 
             the Poverty Headcount Ratio, yet is positively correlated with the Income Share held by the wealthiest 10%.</div>")
    })
    
    #Plot display (similar to other ggplots)
    output$ov_plot <- renderPlot({
      #Formulate data to be plotted
      plot_corrdata <-
        aggrdata[, c(gsub(" ", "_", input$ov_inq_select),
                     gsub(" ", "_", input$ov_econ_select))]
      
      #Then plot an interactive scatterplot using ggplot, along with hover functions
      ggplot(data = plot_corrdata,
             aes(x = plot_corrdata[[1]], y = plot_corrdata[[2]]),
             group = 1) +
        theme(text = element_text(family = "Arial", size = 16)) +
        geom_point(
          shape = 21,
          size = 4,
          show.legend = F,
          fill = "#DB380B"
        ) +
        labs(x = input$ov_inq_select,
             y = paste(input$ov_econ_select,
                       " (million USD)")) +
        geom_smooth(method = 'lm', color = "#0000CC")
    })
    
    #Render the data table depending on the user's choice of statistical variable
    output$ov_table <- DT::renderDataTable({
      if (grepl("P", input$table_val, ignore.case = F)) {
        p_corrdata
      }
      else{
        r_corrdata
      }
    }, options = list(paging = F))
  }

#--------FUNCTIONS--------
#Formulates inqeuality data for display in tooltips & graphs of Country page
formulate.inq.data <- function(input) {
  #First formulate the data to plot
  plot_inqdata <-
    inqdata[, c("Country", gsub(" ", "_", input$inq_select))]
  plot_inqdata <-
    plot_inqdata[plot_inqdata$Country == input$country_select, 2]
  plot_inq_time <-
    inqdata[inqdata$Country == input$country_select, "Year"]
  
  #represents the final formatting of the selected data
  inqplot <- cbind(plot_inq_time, plot_inqdata)
}

#Formulates economic data for display in tooltips & graphs of Country page
formulate.econ.data <- function(input) {
  #First formulate the data to plot
  plot_econdata <-
    econdata[, c("Country", gsub(" ", "_", input$econ_select))]
  plot_econdata <-
    plot_econdata[plot_econdata$Country == input$country_select, 2]
  plot_econ_time <-
    econdata[econdata$Country == input$country_select, "Year"]
  
  #represents the final formatting of the selected data
  econplot <- cbind(plot_econ_time, plot_econdata)
}

#Generates the label text on the leaflet map given the presence or lack of data related to a country
generate.map.text <- function(country_id, data){
  #For nations with data
  if(country_id %in% nzdata$codes){
    text <- paste0(
      "<strong>",
      data$name,
      "</strong><br/><div>Imports from NZ: <font color = \"blue\" face = \"helvetica\">",
      nzdata[data$id == nzdata$codes, "Exports_(thousand_USD)"],
      "</div></font><div>Exports to NZ: <font color = \"blue\" face = \"helvetica\">",
      nzdata[data$id == nzdata$codes, "Imports_(thousand_USD)"],
      "</div></font><div>Trade Balance with NZ: <strong><font color = \"blue\" face = \"helvetica\">",
      (nzdata[data$id == nzdata$codes, "Trade_balance_(thousand_USD)"] *
         -1),
      "</div></font></strong><div>NZ's Influence on Inequality: <font color = \"B90B21\"><strong>Weak</strong></font></div>"
    )
  }
  #For NZ
  else if (country_id == "NZL") {
    text <-
      paste0(
        "<h4><strong>New Zealand</strong></h4><div>Total Imports: <font color = \"#00B200\" face = \"helvetica\">",
        sum(nzdata[, "Imports_(thousand_USD)"], na.rm = T),
        "</font></div><div>Total Exports: <font color = \"#00B200\" face = \"helvetica\">",
        sum(nzdata[, "Exports_(thousand_USD)"], na.rm = T),
        "</font></div><div>Aggregate Trade Balance: <strong><font color = \"00B200\" face = \"helvetica\">",
        sum(nzdata[, "Trade_balance_(thousand_USD)"], na.rm = T),
        "</div></font></strong>"
      )
  }
  #For nations without data
  else{
    text <- paste0("<strong>", data$name, "</strong><br/><div>No trade data available.</div>")
  }
}

#Generates the fill color for a country on the leaflet map
generate.map.fill <- function(country_id, data){
  if(country_id %in% nzdata$codes){
    fill <- "#FFF333"
  }
  else if(country_id == "NZL"){
    fill <- "#3a8de0"
  }
  else{
    fill <- "#A0A0A0"
  }
  
  fill
}

#Generates the highlight color for a country on the leaflet map
generate.map.highlight <- function(country_id, data){
  if(country_id %in% nzdata$codes){
    fill <- "#bfb615"
  }
  else if(country_id == "NZL"){
    fill <- "#66B2FF"
  }
  else{
    fill <- "#8f8f8f"
  }
  
  fill
}

#Generates the text to display for statistics on the country page
generate.stats <- function(textdata, input, display) {
  if (display == "econ") {
    paste0(
      "<h4>",
      input$econ_select,
      #Display Maximum
      "</h4><div> Maximum: <font color = \"blue\" face = \"helvetica\">",
      round(max(textdata[[2]], na.rm = T), digits = 3),
      "</font> (<font color = \"green\" face = \"helvetica\">",
      textdata[which.max(textdata[[2]]), "Year"],
      "</font>)<br/>",
      #Display Mean
      "Mean: <font color = \"blue\" face = \"helvetica\">",
      round(mean(textdata[[2]], na.rm = T), digits = 3),
      "</font><br/>",
      #Display Minimum
      "Minimum: <font color = \"blue\" face = \"helvetica\">",
      round(min(textdata[[2]], na.rm = T), digits = 3),
      "</font> (<font color = \"green\" face = \"helvetica\">",
      textdata[which.min(textdata[[2]]), "Year"],
      "</font>)<br/>",
      #Display SD
      "Standard Deviation: <font color = \"blue\" face = \"helvetica\">",
      round(sd(textdata[[2]], na.rm = T), digits = 3),
      "</font><br/>",
      #Display Variance
      "Variance: <font color = \"blue\" face = \"helvetica\">",
      round(var(textdata[[2]], na.rm = T), digits = 3),
      "</font></div>"
    )
  }
  else{
    paste0(
      "<h4>",
      input$inq_select,
      #Display Maximum
      "</h4><div> Maximum: <font color = \"blue\" face = \"helvetica\">",
      round(max(textdata[[2]], na.rm = T), digits = 3),
      "</font> (<font color = \"green\" face = \"helvetica\">",
      textdata[which.max(textdata[[2]]), "Year"],
      "</font>)<br/>",
      #Display Mean
      "Mean: <font color = \"blue\" face = \"helvetica\">",
      round(mean(textdata[[2]], na.rm = T), digits = 3),
      "</font><br/>",
      #Display Minimum
      "Minimum: <font color = \"blue\" face = \"helvetica\">",
      round(min(textdata[[2]], na.rm = T), digits = 3),
      "</font> (<font color = \"green\" face = \"helvetica\">",
      textdata[which.min(textdata[[2]]), "Year"],
      "</font>)<br/>",
      #Display SD
      "Standard Deviation: <font color = \"blue\" face = \"helvetica\">",
      round(sd(textdata[[2]], na.rm = T), digits = 3),
      "</font><br/>",
      #Display Variance
      "Variance: <font color = \"blue\" face = \"helvetica\">",
      round(var(textdata[[2]], na.rm = T), digits = 3),
      "</font></div>"
    )
  }
}

#Returns an html-tagged string with the strength of a correlation, determined by a constant alpha level
generate.corr.text <- function(r){
  if(r > 0.7 | r < -0.7){
    "<font color = \"059142\">Strong</font>"
  }
  else if(r > -0.25 & r < 0.25){
    "<font color = \"B90B21\">Weak</font>"
  }
  else{
    "<font color = \"#BEBE0F\">Medium</font>"
  }
}

# --------RUN APPLICATION--------
shinyApp(ui = ui, server = server)