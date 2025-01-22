#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Gourds Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          id = 'side-panel',
            checkboxGroupInput("gourd_type",
                               "Select Gourd Type",
                               choices = c("All", gourds |> distinct(Gourd_Type) |> pull(Gourd_Type) |> sort()),
                               select = 1),
            actionButton("reset_input", "Reset"),
          tags$hr(),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Histogram", plotOutput("distPlot"),
                     sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 15)),
            tabPanel("Box Plot", plotOutput("boxPlot")),
            tabPanel("Line Plot", plotOutput("linePlot")),
            tabPanel("Map", leafletOutput("map"),
                     actionButton('plotMap', "Plot Map"))
          )
        )
    )
)
