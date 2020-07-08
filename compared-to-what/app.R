#
# 
#

library(shiny)
library(tidyverse)

# Hard coded built in comparisons
comparisons <- tribble(
    ~name, ~effect_size, ~prevalence, ~source,
    "Lung cancer from smoking", 8, 5, "Smoking is bad for you, Johnson, John (2019), Inoccuous press.",
    "Looking cool from smoking", 1, 5, "Smooking is uncool, Johnson, John (2018), Hamlet General Interest Journal.",
    "Getting pregnant from licking doorknobs", 0.5, 0.5, "You'd think it goes without saying, Johnson, John (2011), Nature."
)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Compared to What?"),

    # Sidebar with inputs for the user's research
    sidebarLayout(
        sidebarPanel(
            helpText("Enter values for your project here. Effect size between 0 and 10, a prevalence in percent, and precision of the effect size between 0 and 5."),
            numericInput("effect_size_input", "Effect size", 0, min = 0, max = 10),
            numericInput("prevalence_input", "Prevalence", 0, min = 0, max = 100, step = 1),
            numericInput("precision", "Precision", 0, min = 0, max = 5),
            actionButton("apply_button", "Apply")
        ),

        # Show a plot of the generated distribution
        # Output: Tabset w/ plot, summary, and table ----
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("Table", tableOutput("table"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    users_data <- reactiveValues(data = NULL)
    
    observeEvent(
        input$apply_button,
        {
            if(!(input$effect_size_input == 0)&
                !(input$prevalence_input == 0)&
                !(input$precision== 0)) {
               users_data$data <- tibble(effect_size = input$effect_size_input,
                      prevalence = input$prevalence_input,
                      error_max = input$effect_size_input + input$precision,
                      error_min = input$effect_size_input - input$precision)
                } else
                {users_data$data <- NULL}
    })
    
    output$plot <- renderPlot({
        p <- ggplot() +
            geom_point(data = comparisons, aes(prevalence, effect_size))
        if (!is.null(users_data$data)) {
            p <- p + geom_point(data = users_data$data, aes(prevalence, effect_size), shape = 1, fill = "white", size = 5)
            p <- p + geom_errorbar(data = users_data$data, aes(x = prevalence, ymax = error_max, ymin = error_min))
         }
        
        print(p)
    })

    # Generate an HTML table view of the data ----
    output$table <- renderTable({
        comparisons
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
