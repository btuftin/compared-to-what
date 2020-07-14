#
# Compared to What? is an app for generating plots comparing prevalence
# and effect size for your research with built in and custom benchmark
# studies.
#
# The goal is to encourage better impact analysis and understanding of 
# effect sizes.
#
# It was created using the shiny package.

library(shiny)
library(tidyverse)
library(DT)


# The built in comparisons are in the file data/comparisons.csv
# Column headers are  "label","risk_ratio","hazard_ratio",
#                       "odds_ratio","cohens","risk_diff",
#                       "original","prev_outc","prev_exp","field","design","ref"
file_comparisons <- "data/comparisons.csv"
#file.exists(file_comparisons)
comparisons <- read_csv(file_comparisons)

# Various labels and formatting are in the file data/labels.csv
# Column headers are "label_id","label","flags"
file_labels <- "data/labels.csv"
label_texts <- read_csv(file_labels)

# Various aspects of the plot are defined in this theme
plot_theme <- theme(aspect.ratio = 1) + theme_bw()

# Some reused arguments. OBS! The values have to be the same as the column
# headers in comparisons and user_data
effect_size_choices = c(`Cohen's D`= "cohens",
                        `Hazard ratio` = "hr",
                        `Odds ratio` = "or",
                        `Risk difference` = "rd",
                        `Risk ratio` = "rr")

prevalence_choices = c(`Prevalence of Outcome` = "prev_outc",
                       `Prevalence of Exposure` = "prev_exp")

# Fetch study design studies from the comparisons tibble
study_design_choices = comparisons$design

# Fetch fields of study from the tibble and add "Custom" for custom reference
study_field_choices = c(comparisons$field, "Custom")

# User entered effect sizes need to be converted to all other effect size types
# allowed for plotting
effect_size_convert <- function(type_out, type_in, value){
    if (type_out == type_in)
        return (value)
    else
        #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
        # Currently there is no conversion! Only Zuul!
        return (value * runif(1))
}

# The top level in the user interface is a navbarPage. It creates a navigation
# bar on the top of the page with a tab for each of the tabPanels included
ui <- navbarPage("Compared to What?",
                 
     # The first tabpanel is the Intro panel, which has some introduction material
     # and basic top level instructions for use.
     tabPanel("Intro",
              h1("Introduction"),
              p(HTML("<b>Compared to What?</b>"), "is an app for generating plots comparing prevalence and effect size for an index association with built in and custom references. It is currently in pre-alpha, so don't use it in any manner that requires it to still exist in the same form (or at all) tomorrow."),
              p("The goal is to encourage better impact analysis and understanding of effect sizes."),
              p("It was created using the shiny package and also uses the tidyverse, and DT packages."),
              h2("Do this"),
              p("This is how you use it. Lorem Ipsum dolor sit amet.")),
             
     # The second tabpanel is where the user enters data for the index associate
     # to be compared with the references
     tabPanel("Index Association",
              fluidPage(
                  fluidRow(column(width = 12,
                                  helpText("Enter values for your project here. An effect size as one of risk ratio, odds ration, hazard ratio etc. (chose the applicable one from the menu), prevalences of outcome and exposure in percent, and the upper and lower bound for the confidence interval for this effect"))),
                  fluidRow(column(width = 4,
                                  helpText(HTML("<b>Effect size</b>"))),
                           column(width = 4,
                                  helpText(HTML("<b>Prevalence</b>"))),
                           column(width = 4,
                                  helpText(HTML("<b>Confidence interval</b>")))),
                  fluidRow(column(width = 2,
                          numericInput("effect_size_input", "Value", 0, min = 0, max = 10)),
                       column(width = 2,
                          selectInput("effect_size_type_input",
                                      "Type",
                                      choices = effect_size_choices)),
                       column(width = 2,
                          numericInput("prev_exp_input", 
                                       "of Exposure", 
                                       0, min = 0, max = 100, step = 1)),
                       column(width = 2,
                              numericInput("prev_outc_input", 
                                           "of Outcome", 
                                           0, min = 0, max = 100, step = 1)),
                       column(width = 2,
                              numericInput("lower_bound", 
                                           "Lower bound", 
                                           0, min = 0, max = 100, step = 1)),
                       column(width = 2,
                              numericInput("upper_bound", 
                                           "Upper bound", 
                                           0, min = 0, max = 100, step = 1))),
                  fluidRow(column(width = 2,
                       actionButton("apply_button", "Apply")
                                   )))),
                          
    # The third tabpanel shows the plot and lets the user chose what variables
    # to use for each axis.
    tabPanel("Plot",
           sidebarLayout(
                   sidebarPanel(
                       HTML("<p><b>Effect size</b></p>"),
                       p("Plot effect size as:"),
                       selectInput("effect_size_type_to_plot",
                                   NULL,
                                   choices = effect_size_choices),
                       HTML("<p><b>Prevalence</b></p>"),
                       p("Plot prevalence as:"),
                       radioButtons("prevalence_type",
                                    NULL,
                                    choices = prevalence_choices),
                       p("To filter references on field or study design, use controls on the References page")
                   ),
                                       
                   mainPanel(plotOutput("plot")))),

    
     tabPanel("References",
            fluidPage(
                fluidRow(column(width = 12,
                                helpText("Filter on study design or field of study."),
                                helpText("All custom references have 'Custom' as their 'field of study'.")),
                         column(width = 2,
                                selectInput("designs_chosen",
                                            "Design(s)",
                                            multiple = TRUE,
                                            choices = study_design_choices)),
                         column(width = 2,
                                selectInput("fields_chosen",
                                            "Field(s)",
                                            multiple = TRUE,
                                            choices = study_field_choices))),
                fluidRow(column(width = 12,
                                dataTableOutput("table"))),
                fluidRow(column(width = 12,
                                helpText("To add a custom references, fill in all fields and press 'Apply'. To delete an added custom reference, select the table row and press 'Delete'."))),
                fluidRow(column(width = 2,
                                helpText("")),
                         column(width = 4,
                                helpText(HTML("<b>Effect size</b>"))),
                         column(width = 4,
                                helpText(HTML("<b>Prevalence</b>"))),
                         column(width = 2,
                                helpText(HTML("Study")))),
                fluidRow(column(width = 2,
                                textInput("ref_label", "Label",
                                          placeholder = "Effect of locusts on house fires.")),
                         column(width = 2,
                                numericInput("effect_size_in_custom", 
                                             "Value", 
                                             0, min = 0, max = 10)),
                         column(width = 2,
                                selectInput("effect_size_t_custom",
                                            "Type",
                                            choices = effect_size_choices)),
                         column(width = 2,
                                numericInput("prev_exp_custom", 
                                             "of Exposure", 
                                             0, min = 0, max = 100, step = 1)),
                         column(width = 2,
                                numericInput("prev_outc_custom", 
                                             "of Outcome", 
                                             0, min = 0, max = 100, step = 1)),
                         column(width = 2,
                                selectInput("study_design", 
                                             "design", 
                                             choices = study_design_choices)),
                fluidRow(column(width = 2,
                                offset = 4,
                                actionButton("custom_apply", "Apply")),
                         column(width = 2,
                                actionButton("custom_delete", "Delete"))
                )
                )
            )
     )
)




# Define server logic required to draw a histogram
server <- function(input, output) {

    all_references <- reactiveValues(data = comparisons)
    
    references <- reactive({
        designs <- if(!is.null(input$designs_chosen)) input$designs_chosen 
                    else study_design_choices
        fields <- if(!is.null(input$fields_chosen)) input$fields_chosen
                    else study_field_choices
        all_references$data %>% filter(design %in% designs & field %in% fields) %>%
            mutate(effect_size = pull(., input$effect_size_type_to_plot)) %>%
            mutate(prevalence = pull(., input$prevalence_type))
    })
    
    users_data <- reactiveValues(data = NULL)
    
    
    # When the users clicks the apply button the Index Association should be
    # updated, if the values aren't the default 0s
    # Currently it doesn't call the effect_size_convert for recording the upper
    # and lower bound, which will be required.
    observeEvent(
        input$apply_button,
        {
            users_data$data <- NULL
            if(!(input$effect_size_input == 0)&
                                !(input$prev_exp_input == 0)&
                                !(input$prev_outc_input == 0)&
                                !(input$upper_bound <= input$lower_bound))
            {
               temp <- tibble(prev_exp = input$prev_exp_input,
                                         prev_outc = input$prev_outc_input,
                                         error_max = input$upper_bound,
                                         error_min = input$lower_bound)
               temp <- temp %>% add_column(cohens = effect_size_convert("cohens",
                       input$effect_size_type_input, input$effect_size_input))
               temp <- temp %>% add_column(hr = effect_size_convert("hr",
                       input$effect_size_type_input, input$effect_size_input))
               temp <- temp %>% add_column(or = effect_size_convert("or",
                       input$effect_size_type_input, input$effect_size_input))
               temp <- temp %>% add_column(rd = effect_size_convert("rd",
                       input$effect_size_type_input, input$effect_size_input))
               temp <- temp %>% add_column(rr = effect_size_convert("rr",
                       input$effect_size_type_input, input$effect_size_input))
               users_data$data <- temp
               temp <- NULL
               removeNotification(id = "index_error")
            } else {
                showNotification("Invalid or missing data",
                                 duration = NULL,
                                 id = "index_error", 
                                 type = "error")
            }
        }
    )

    # When the users clicks the apply button on the table tab a custom reference
    # should be added. This mimics a lot of the code for the index association
    observeEvent(
        input$custom_apply,
        {
            if(!(is.null(input$ref_label))&
               !(input$prev_exp_custom == 0)&
               !(input$prev_outc_custom == 0)&
               !(is.null(input$study_design)))
            {
                all_references$data <- all_references$data %>%
                    add_row(
                        label = input$ref_label,
                        rr = effect_size_convert("rr",
                                                 input$effect_size_t_custom,
                                                 input$effect_size_in_custom),
                        hr = effect_size_convert("hr",
                                                 input$effect_size_t_custom,
                                                 input$effect_size_in_custom),
                        or = effect_size_convert("or",
                                                 input$effect_size_t_custom,
                                                 input$effect_size_in_custom),
                        cohens = effect_size_convert("cohens",
                                                     input$effect_size_t_custom,
                                                     input$effect_size_in_custom),
                        rd = effect_size_convert("rd",
                                                 input$effect_size_t_custom,
                                                 input$effect_size_in_custom),
                        orig. = input$effect_size_t_custom,
                        prev_outc = input$prev_outc_custom,
                        prev_exp = input$prev_exp_custom,
                        field = "Custom",
                        design = input$study_design,
                        ref = "User supplied")
                removeNotification(id = "custom_error")
            } else {
                showNotification("Invalid or missing data",
                                 duration = NULL,
                                 id = "custom_error", 
                                 type = "error")
            }
        }
    )
    
    # When the users clicks the apply button on the table tab a custom reference
    # should be added. This mimics a lot of the code for the index association
    observeEvent(
        input$custom_delete,
        {
            all_references$data <- all_references$data %>%
                    filter(label != input$ref_label)
        }
    )
        
    # The current index association to plot depends on what effect
    # size has been chosen on the plot type
    # This needs to be updated to include the upper and lower bound
    # since they too wil change with the effect size type.
    # That will be a whole other mess.
    index_association <- reactive({
        users_data$data %>% mutate(effect_size = 
                                       pull(., input$effect_size_type_to_plot)) %>%
            mutate(prevalence = pull(., input$prevalence_type))
    })
        
    output$plot <- renderPlot({
        p <- ggplot() +
            geom_point(data = references(), aes(prevalence, effect_size))
        if (!is.null(users_data$data)) {
            p <- p + geom_point(data = index_association(), aes(prevalence, effect_size), shape = 1, fill = "white", size = 5)
            p <- p + geom_errorbar(data = index_association(), aes(x = prevalence, ymax = error_max, ymin = error_min))
         }
        p <- p + plot_theme
        
        print(p)
    })

    # Generate an HTML table view of the data ----
    # Commented out to examine the derived table for debugging
    output$table <- renderDT(datatable(references()[1:12],
                                    options = list(autoWidth = TRUE,
                                                   scrollX = TRUE,
                                                   columnDefs = list(
                                                       list(targets = 0,
                                                            visible = FALSE),
                                                       list(targets = 1,
                                                            width = '200px'),
                                                       list( targets = 12,
                                                             width = '300px'))
                                                   )
                                    )
                             )
}

# Run the application 
shinyApp(ui = ui, server = server)
