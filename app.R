#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(highcharter)
library(shinyWidgets)
library(DT)


# I really shouldn't have to write a function for this

is_not_null_and_eq <- function(value, eq){
  if (!is.null(value)){
    return(value == eq)
  }else{
    return(FALSE)
  }
}

data_generator <- function(region = 1:8){
  
  region_name <- case_when(
    region == 1 ~ 'Northland',
    region == 2 ~ 'Waikato',
    region == 3 ~ 'Bay of Plenty',
    region == 4 ~ 'Taranaki',
    region == 5 ~ 'Lower North Island',
    region == 6 ~ 'West coast - Top of the South',
    region == 7 ~ 'Canterbury',
    region == 8 ~ 'Otago - Southland')
  
  Years <- 2002:2023
  
  prop_dairy_cattle <- c(0.07, 0.27, 0.08, 0.1, 0.08, 0.06, 0.24, 0.1)
  prop_beef_cattle <- c(0.09, 0.22, 0.11, 0.14, 0.10, 0.07, 0.15, 0.12)
  prop_sheep <- c(0.08, 0.16, 0.10, 0.12, 0.14, 0.09, 0.11, 0.2) 
  
  dataset <- region %>%
    imap(
      function(x, y){
        Season <- paste0(Years, '-', str_sub(Years+1, 3, 4))
        
        beef_cattle <- vector('numeric', length = length(Years))
        dairy_cattle <- vector('numeric', length = length(Years))
        sheep <- vector('numeric', length = length(Years))
        
        beef_cattle[1] <- 3950000
        dairy_cattle[1] <- 1250000
        sheep[1] <- 5800000
        
        for(i in 2:length(Years)){
          beef_cattle[i] <- beef_cattle[i - 1] + 
            (Years[i] <= 2012) * (- 100000 + rnorm(1, 0, 10000)) +
            (Years[i] > 2005 & Years[i] <= 2017) * (-1000 + rnorm(1, 0, 8000)) +
            (Years[i] > 2017) * abs(rnorm(1, 0, 10000))      
          dairy_cattle[i] <- dairy_cattle[i - 1] + 
            (Years[i] <= 2012) * (100000 + rnorm(1, 0, 10000)) +
            (Years[i] > 2012 & Years[i] <= 2017) * rnorm(1, 0, 8000) +
            (Years[i] > 2017) * -abs(rnorm(1, 0, 50000))
          sheep[i] <- sheep[i - 1] - 7500 + rnorm(1, -50000, 60000)
        }
        
        beef_cattle <- round(beef_cattle * prop_beef_cattle[y])
        dairy_cattle <- round(dairy_cattle * prop_dairy_cattle[y])
        sheep <- round(sheep * prop_sheep[y])
        
        return(
          tibble(
            `Season` = Season,
            `Region` = region_name[y],
            `Beef cattle` = beef_cattle,
            `Dairy cattle` = dairy_cattle,
            `Sheep` = sheep
          )
        )
        
      }
    ) %>%
    bind_rows()
  
  return(dataset)
  
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Stat's NZ Land Use Area Example"),

    # Sidebar with a slider input for number of bins 
    navbarPage(
      "Farm land use area",
      id = "tabs",
      position = "static-top",
      # Begin with a tab panel containing three tabs: "Graph", "Table" and "Download"
      tabPanel("Graph", 
               # Side bar - only appears for Graph
               sidebarLayout(
                 sidebarPanel(
                   # This creates the radio buttons for Over time and By Region
                   radioButtons("compare", "Compare:", choices = c("Over time", "By region"), selected = "Over time"),
                   # Creates the selection of land_use. Changes between checkbox and select box
                   # depending on the choice above
                   uiOutput('land_use'),
                   # A conditional panel is used so this option only appears if
                   # the condition (the compare radio button is set to "By region")
                   # is met.
                   conditionalPanel(
                     condition = "input.compare == 'By region'",
                     # Generate the year slider. I create the year slider on
                     # the server side because the years are dependent on the
                     # choice of data
                     uiOutput("year_slider"),
                     # Use a flex display to allow the text and div to sit alongside
                     # each other
                     div(style = "display: flex;",
                         strong("Change:"),
                         # Use the options in the style to make the prettySwitch 
                         # vertically aligned in the middle
                         div(style = "display: flex; align-items: center; justify-content: center;",
                             prettySwitch("change", label = NULL, status = "primary", width = "96px", bigger = TRUE, fill = TRUE)),
                         
                     ),
                     # Output the units ui - this only shows if the toggle switch
                     # (prettySwitch) above is turned on. This check is done on
                     # the server side
                     uiOutput('units')
                   ),
                   # This conditional panel contains the radio box allowing you
                   # to choose between New Zealand or select a region
                   # The condition requires "Over time" to be selected in the
                   # compare checkbox
                   conditionalPanel(
                     condition = "input.compare == 'Over time'",
                     # Create the radio buttons allowing the user to choose between
                     # NZ and a region
                     radioButtons("area", "Area:", choices = c("New Zealand", "Select a region"), selected = "New Zealand")
                   ),
                   # This conditional panel requires that the compare checkbox is
                   # set to Over time and the Area checkbox is set to 'Select a region'.
                   conditionalPanel(
                     condition = "input.area == 'Select a region' & input.compare == 'Over time'",
                     # Creates a select box allowing the users to choose between the regions
                     selectInput("area_select", label = "Region", 
                                 choices = list("Northland", "Waikato", "Bay of Plenty", "Taranaki", "Lower North Island",
                                                "West Coast - Top of the South", "Canterbury", "Otago - Southland"), 
                                 selected = "Northland")
                   ),
                   # A nice little text tag at the bottom like theirs
                   tags$p("Data source: Example, based on the Agricultural Production Survey from Stats NZ"),
                   width = 4
                 ),
                 # The main panel of the "Graph" table shows the plot.
                 # You may want to consider improving the formatting here - I haven't
                 # messed around with highcharts margins etc. to make the app
                 # appear cleaner
                 mainPanel(
                   highchartOutput("plot")
                 )
               )
      ),
      # These are a bit easier - in the case of table we show the dataTable and in the case
      # of the download, we show a download button.
      tabPanel("Table", dataTableOutput("regional_table")),
      tabPanel("Download", downloadButton("download_data", label = "Download Data", icon = icon("download")), icon = icon('download'))

    )
)


server <- function(input, output, session) {

    ## Data Generation process --------------------------------------------------  
  
    data <- data_generator() 
    
    ## Data formatting ---------------------------------------------------------
    # Process:
    # 1. Generate the national values (bind_rows, summarise)
    # 2. Pivot the columns to stack the land use variables by rows (pivot_longer)
    
    data <- data %>%
      bind_rows(
        data %>%
          group_by(`Season`) %>%
          summarise(
            `Region` = 'New Zealand',
            across(-`Region`, ~ sum(.x)))
      ) %>% 
      pivot_longer(cols = -c(`Season`, `Region`),
                   names_to = "Land use",
                   values_to = "Values")
    
    
    ## Render the year slider --------------------------------------------------
    # The year slide needs to be adaptable to the number of seasons in the data.
    # Construct the widget on the server side (renderUI) and pass it to the UI
    # via uiOutput
    
    # Pass min year to UI
    output$year_slider <- renderUI({
      
      # Extract the minimum and maximum season in the data
      min_year <- min(as.numeric(substr(data$`Season`, 1, 4)))
      max_year <- max(as.numeric(substr(data$`Season`, 1, 4)))
      
      # Create the slider input
      sliderInput("years", "Years:", min = min_year, max = max_year, value = c(min_year, max_year), sep = "")
    })
    
    
    ## Render the land_use widget  --------------------------------------------
    # If the radio box is set to "Over time", then produce a select box (multiple selectable land use options)
    # (updateCheckboxGroupInput). If the radio box is set to "By region", then
    # produce a radio box (only one selectable land use option).
    
    # RenderUI generates the widget to be created by the uiOutput tag in the client
    
    output$land_use <- renderUI({
      if(input$compare == "By region"){
        radioButtons("land_use", 
                     "Farm land use:", 
                     choices = c("Beef cattle", "Dairy cattle", "Sheep"), 
                     selected = "Beef cattle")
      }else{
        checkboxGroupInput("land_use", 
                           "Farm land use:", 
                           choices = c("Beef cattle", "Dairy cattle", "Sheep"), 
                           selected = c("Beef cattle", "Dairy cattle", "Sheep"))
      }
    })
    
    ## Render the units widget -------------------------------------------------
    # Conditional panels in this case have not worked with the units widget. For
    # some reason, it doesn't correctly evaluate the value of input.change (in the UI)
    # to determine if it should show or not. Doing it from the server end will
    # ensure that this works
    
    output$units <- renderUI({
      if (input$change){
        radioButtons("units", "Units:", choices = c("Hectares", "Percent"), selected = "Hectares")
      }
    })
    

    ## Reactive command for data_generation_id ---------------------------------
    # I generate the value of data_generation_id here so I can use it in multiple
    # commands, and then it can also react to changes.
    
    # This sets data_generation_id based on the data procedure that is required.
    # This will also be used to determine the graph type.
    data_generation_id <- reactive({
      
      case_when(
        is_not_null_and_eq(input$compare, 'By region') & 
          is_not_null_and_eq(input$change, TRUE) &
          is_not_null_and_eq(input$units, 'Hectares') ~ 1, # "By Region", Change = TRUE, Hectares
        
        is_not_null_and_eq(input$compare, 'By region') & 
          is_not_null_and_eq(input$change, TRUE) &
          is_not_null_and_eq(input$units, 'Percent') ~ 2, # "By Region", Change = TRUE, Percent    
        
        is_not_null_and_eq(input$compare, 'By region') & 
          is_not_null_and_eq(input$change, FALSE) ~ 3, # "By Region", Change = FALSE
        
        is_not_null_and_eq(input$compare, 'Over time') &
          is_not_null_and_eq(input$area, "New Zealand") ~ 4, # Over time - all of New Zealand
        
        is_not_null_and_eq(input$compare, 'Over time') &
          is_not_null_and_eq(input$area, "Select a region") ~ 5, # Over time - select region
        
        TRUE ~ 4 # Default to over time - all of NZ
      )
      
    })
    
    
    ## Observe for tabs --------------------------------------------------------
    # Because the code further down requires the value of tabs, we need to make sure
    # it is observed otherwise it may throw an error under certina circumstances
    
    navbar_selection <- reactive({
      input$tabs
    })
    
    ## Reactive command for data -----------------------------------------------
    # A react command is used to allow the dataset to react to the different
    # input choices
    # Process:
    # 1. Generate the "data_selected" variable which filters the data by
    # choice of land use
    # 2. Depending on the value of data_generation_id, generate the required
    # dataset.
    data_selected <- reactive({
      
      if (navbar_selection() == "Graph"){
      
        data_selected <- data %>% filter(`Land use` %in% input$land_use)
        
        if (data_generation_id() == 1){
          
          # By Region, change = TRUE, Hectares
          
          data_selected %>%
            mutate(`Season number` = as.numeric(str_sub(`Season`, 1, 4))) %>%
            filter(`Season number` == input$years[1] | `Season number` == input$years[2]) %>%
            filter(`Region` != "New Zealand") %>%
            group_by(`Region`) %>%
            summarise(`Values` = last(`Values`) - first(`Values`)) %>%
            ungroup()
          
        }else if (data_generation_id() == 2){
          
          # By Region, change = TRUE, Percent
          
          data_selected %>%
            mutate(`Season number` = as.numeric(str_sub(`Season`, 1, 4))) %>%
            filter(`Season number` == input$years[1] | `Season number` == input$years[2]) %>%
            filter(`Region` != "New Zealand") %>%
            group_by(`Region`) %>%
            summarise(`Values` = 100 * (last(`Values`) - first(`Values`)) / first(`Values`)) %>%
            ungroup()
          
          
        }else if (data_generation_id() == 3){
          
          # By Region, change = FALSE
          
          data_selected %>%
            mutate(`Season number` = as.numeric(str_sub(`Season`, 1, 4))) %>%
            filter(`Season number` == input$years[1] | `Season number` == input$years[2]) %>%
            filter(`Region` != "New Zealand")
          
        }else if (data_generation_id() == 4){
          
          data_selected %>% 
            filter(`Region` == "New Zealand")
          
          
        }else if (data_generation_id() == 5){
          
          data_selected %>% 
            filter(`Region` == input$area_select)
          
        }
      }else if (navbar_selection() == "Table"){
        
        # In this case we just want the regional data values, but it needs to be pivoted
        # wider by season. 
        
        data %>%
          filter(`Region` != "New Zealand") %>%
          pivot_wider(
            names_from = 'Land use',
            values_from = 'Values'
          )
        
      }
      
    })
    
  
    
    
    
    ## Generate the plots ------------------------------------------------------
    
    # I would recommend using Plotly rather than Highcharts. I have done this
    # because the Economic Farm Dashboard is using Highcharts. Plotly is significantly
    # more user-friendly and adaptable in my opinion, even if Highcharts offers more
    # ability to customise things with Javascript.
    
    output$plot <- renderHighchart({
      
      # require the input$compare value
      req(input$compare)
      
      # Define colors for each land use
      colors <- c("#69BE28", "#353735", "#009AA6")
      
      # NOTE: You could alternatively do this in an observeEvent(). The difference
      # is you would have to render the chart there and then do renderHighchart(chart_var())
      # This is a quick (but I think valid) hack to avoid that. 
      if (navbar_selection() != "Graph"){
        return() # We just escape this event without drawing
      }
      
      if (between(data_generation_id(), 4, 5)){
        
        # Render the chart that shows a line graph nationally
        # Create Highcharts object
        highchart() %>%
          hc_title(text = paste0("Farm ", paste(input$land_use, collapse = " and "), " land use area in New Zealand 2002 - 2023")) %>%
          hc_add_series(data_selected(), "line", hcaes(x = `Season`, y = `Values`, group = `Land use`),
                        line = list(color = colors, width = 2)) %>%#,
          hc_xAxis(categories = unique(data_selected()$`Season`)) %>%
          hc_colors(colors) %>%
          hc_legend(enabled = TRUE) %>%
          hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
          hc_yAxis(labels = list(format = '{value:,f}'))
          
        
      }else if (between(data_generation_id(), 1, 2)){
        
        # Render bar plot that shows the changes by region
        # The tooltip has to change depending on if percentages or actual values
        # have been shown.
        highchart() %>%
          hc_title(text = paste0('Farm ', input$land_use, " land use area in New Zealand ", input$years[1], ' - ', input$years[2])) %>%
          hc_add_series(data_selected(), "bar", hcaes(y = `Values`, x = `Region`)) %>%
          hc_xAxis(categories = data_selected()$`Region`) %>%
          hc_yAxis(
            min = -max(abs(data_selected()$`Values`)),
            max = max(abs(data_selected()$`Values`))
          ) %>%
          hc_colors('#69BE28') %>%
          hc_legend(enabled = FALSE) %>%
          hc_tooltip(shared = FALSE, crosshairs = TRUE, useHTML = TRUE,
                     formatter = JS(
                       str_c("
                       function(){
                        outHTML = '<b>Region</b>: ' + this.point.Region + '<br>' +
                                  '<b>Change</b>: ' +",
                             ifelse(data_generation_id() == 2,
                                    "Math.round(this.y, 2) + '%';\n",
                                    "Highcharts.numberFormat(this.y, 0) + ' ha';\n"),
                             "return(outHTML)
                        }"))) %>%
          hc_yAxis(labels = list(format = ifelse(data_generation_id() == 2, '{value}%', NA)))
        
        

        
        
      }else if (data_generation_id() == 3){
        
        # Render bar plot that shows the changes by region
        highchart() %>%
          hc_title(text = paste0('Farm ', input$land_use, " land use area in New Zealand ", input$years[1], ' - ', input$years[2])) %>%
          hc_add_series(data_selected(), "bar", hcaes(y = `Values`, x = `Region`, group = `Season`)) %>%
          hc_xAxis(categories = unique(data_selected()$`Region`)) %>%
          hc_yAxis(
            min = -max(abs(data_selected()$`Values`)),
            max = max(abs(data_selected()$`Values`))
          ) %>%
          hc_colors(colors = colors[1:2]) %>%
          hc_legend(enabled = TRUE) %>%
          hc_tooltip(shared = FALSE, crosshairs = TRUE)   
        
      }
      

      
    })
    
    
    ## Generate the regional table ---------------------------------------------
    
    output$regional_table <- renderDataTable({
      
      # NOTE: You could alternatively do this in an observeEvent(). The difference
      # is you would have to render the chart there and then do renderHighchart(chart_var())
      # This is a quick (but I think valid) hack to avoid that. 
      if (navbar_selection() != "Table"){
        return() # We just escape this event without drawing
      }
      
      # Create the data table. Replicate Statistics NZ choice of including a filter at the top. I did leave the
      # search bar in (serachable).
      datatable(data_selected(), options = list(searchable = TRUE, lengthChange = FALSE), filter="top")
    })
    
    
    ## Create the download Data handler ----------------------------------------
    # This will generate an action when the download button is pressed by the user.
    # It will take the content and generate a downloadable CSV from it.
    
    output$download_data <- downloadHandler(
      filename = function(){
        return(paste0('Land-use-table-',  lubridate::today(), '.csv'))
      },
      content = function(con){
        write_csv(data, con)
      }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
