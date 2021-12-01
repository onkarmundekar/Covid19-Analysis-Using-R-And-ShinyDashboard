library(shiny)
library(plotly)



source("C:/Users/munde/Documents/BIDMiniProject/corrector.R")


covid_cor <- Corrector$new()



ui <- fluidPage(
    
    ## Application title
    titlePanel("COVID-19 Case & Death Report Number"),
    
    br(),
    
    
    sidebarLayout(
        
        ## Sidebar -------------------------------------------------------------- ##
        sidebarPanel(
            selectInput("act_country", "Country to correct", covid_cor$countries, "USA"),
            selectInput("ref_country", "Reference country", covid_cor$countries, "South Korea"),
            br(),
           
        ),
        
        ## ---------------------------------------------------------------------- ##
        
        # Show a plot of the generated distribution
        mainPanel(
            br(),
            fluidRow(
                column(6,
                       plotlyOutput("act_country_plot")
                ),
                column(6, 
                       plotlyOutput("ref_country_plot")
                )
            ),
            br(), br(),
            fluidRow(
                column(6,
                       plotlyOutput("corrected_cases_plot")
                ),
                column(6,
                       plotlyOutput("dr_plot")
                )
            ),
            br(), br(),
            fluidRow(
                column(6,
                       plotlyOutput("stats_fixed_cases_plot")
                ),
                column(6,
                       plotlyOutput("stats_fixed_death_plot")
                )
            ),
            br(), br(),
            fluidRow(
                column(6,
                       plotlyOutput("demo_plot")
                ),
                column(6,
                       plotlyOutput("demo_death_plot")
                )
            ),
            br(), br()
        )
    )
)

# Define server logic required to draw the plots
server <- function(input, output) {
    
    output$act_country_plot <- renderPlotly({
        covid_cor$get_country_plot_data(input$act_country) %>% 
            plot_ly(x = ~date, y = ~cases, type = "scatter", mode = "lines+markers", color = I("dark green"), name = "Cases") %>% 
            add_trace(x = ~date, y = ~recovered, type = "scatter", mode = "lines+markers", color = I("blue"), name = "Recovered") %>% 
            add_trace(x = ~date, y = ~deaths, type = "scatter", mode = "lines+markers", color = I("red"), name = "Deaths", yaxis = "y2") %>% 
            layout(title = list(text = input$act_country), yaxis2 = list(overlaying = "y", side = "right", automargin = T), yaxis = list(title = "Cumulative Report Number"), xaxis = list(title = "Date"), legend = list(x = 0.05, y = 0.95))
    })
    output$ref_country_plot <- renderPlotly({
        covid_cor$get_country_plot_data(input$ref_country) %>% 
            plot_ly(x = ~date, y = ~cases, type = "scatter", mode = "lines+markers", color = I("dark green"), name = "Cases") %>% 
            add_trace(x = ~date, y = ~recovered, type = "scatter", mode = "lines+markers", color = I("blue"), name = "Recovered") %>% 
            add_trace(x = ~date, y = ~deaths, type = "scatter", mode = "lines+markers", color = I("red"), name = "Deaths", yaxis = "y2") %>% 
            layout(title = list(text = paste0(input$ref_country, " (as reference)")), yaxis2 = list(overlaying = "y", side = "right", automargin = T), yaxis = list(title = "Cumulative Report Number"), xaxis = list(title = "Date"), legend = list(x = 0.05, y = 0.95))
    })
    
    
    output$corrected_cases_plot <- renderPlotly({
        covid_cor$new_correction(active_country = input$act_country, reference_country = input$ref_country)
        covid_cor$get_corrected_cases_plot_data() %>% 
            plot_ly(x = ~date, y = ~cases, type = "scatter", mode = "lines+markers", color = I("dark green"), name = "Cases") %>% 
            add_trace(x = ~date, y = ~potential_cases, type = "scatter", mode = "lines+markers", color = I("light green"), name = "Adj. Cases") %>% 
            layout(title = list(text = paste0(input$act_country, " Cases (Adj. via ", input$ref_country, ")")), yaxis = list(title = "Cumulative Report Number"), xaxis = list(title = "Date"), legend = list(x = 0.05, y = 0.95))
    })
    
    
    output$dr_plot <- renderPlotly({
        covid_cor$compare_deathrates(active_country = input$act_country, reference_country = input$ref_country) %>% 
            plot_ly(x = ~date, y = ~dr_ref, type = "scatter", mode = "lines+markers", color = I("grey"), name = input$ref_country) %>% 
            add_trace(x = ~date, y = ~dr_act, type = "scatter", mode = "lines+markers", color = I("red"), name = input$act_country) %>% 
            layout(title = list(text = paste0("Death Rate Comparison: ", input$act_country, " & ", input$ref_country)), yaxis = list(title = "Death Rate"), xaxis = list(title = "Date"), showlegend = F)
    })
    
    
    
    
  
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
/