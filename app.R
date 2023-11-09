#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("ct-util.R")
max_num_studies = 1000

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("brief_title_kw", "Brief title keywords"),
      selectInput("source_class", label = h3("Sponsor Types"),
                         choices=list(
                                      "Federal" = "FED",
                                      "Individual" = "INDIV",
                                      "Industry" = "INDUSTRY",
                                      "Network" = "NETWORK", "NIH" = "NIH",
                                      "Other" = "OTHER",
                                      "Other gov" = "OTHER_GOV",
                                      "Unknown" = "Unknown"),multiple = TRUE),
      
      dateRangeInput("dates", label = h3("Date Range")),
      sliderInput("slider", label = h3("Age Range"), min = 0, 
                           max = 100, value = c(0, 100)),
      checkboxGroupInput("sponsor", label = h3("Lead or Collaborator"), 
                         choices = list("lead" = "lead", "collaborator" = "collaborator"),
                         ),
      checkboxGroupInput("study_type", label = h3("Study Type"), 
                         choices = list("Interventional" = "Interventional",
                                        "Expanded Access" = "Expanded Access",
                                        "Observational" = "Observational",
                                        "Observational [Patient Registry]" = "Observational [Patient Registry]"),
      )
    ),
    

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
         type = "tabs",
         tabPanel("Phase", plotOutput("phase_plot")),
         tabPanel("Concurrent", plotOutput("concurrent_plot")),
         tabPanel("Conditions", plotOutput("conditions_plot")),
         tabPanel("Countries", plotOutput("countries_plot")),
         tabPanel("Status", plotOutput("status_plot"))
       ),
      dataTableOutput("trial_table")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  get_studies = reactive({
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
           strsplit(",") |>
           unlist() |>
           trimws()
      ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
    } else {
      ret = studies
    }
   if(!is.null(input$source_class)){
    ret = ret |> filter(source_class %in% !!input$source_class)
   }
    input_start <- input$dates[1]
    input_end <- input$dates[2]
    if(!is.null(input$dates)){
      ret = ret |> filter(start_date <= input_start, completion_date >= input_end)
    }
    age_start <- input$slider[1]
    age_end <- input$slider[2]
    if(!is.null(input$slider2)){
      ret = ret |> filter(min_age <= age_end | max_age >= age_start)
    }
    if(!is.null(input$sponsor)){
      ret = ret |> filter(lead_or_collaborator %in% !!input$sponsor)
    }
    if(!is.null(input$study_type)){
      ret = ret |> filter(study_type %in% !!input$study_type)
    }
    
    ret |>
      head(max_num_studies) |>
      collect()
  })
  
  output$phase_plot = renderPlot({
    get_studies() |>
      plot_phase_histogram()
  })

  output$concurrent_plot = renderPlot({
    get_studies() |>
      plot_concurrent_studies()
  })
  
  output$conditions_plot = renderPlot({
    get_studies() |>
      plot_conditions_histogram()
  })
  
  output$countries_plot = renderPlot({
    get_studies() |>
      plot_countries_map()
  })
  
  output$status_plot = renderPlot({
    get_studies() |>
      plot_status_piechart()
  })

  output$trial_table = renderDataTable({
    get_studies() |> 
      head(max_num_studies) |> 
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
