#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({
        
    validate(
          need(input$gourd_type != "", "Please select a gourd.")
        )
        
    if ("All" %in% input$gourd_type) {
        gourds |> 
        ggplot(aes(x=weight_lbs, color = Gourd_Type)) + 
        geom_histogram(bins = input$bins) + 
        labs(title = "Weight Distrubution of All Gourds") +
        xlab("weight (lbs)")
    }
    else{
        gourds |> 
          filter(Gourd_Type %in% c(input$gourd_type)) |> 
          ggplot(aes(x=weight_lbs, color = Gourd_Type)) + 
          geom_histogram(bins = input$bins) + 
          labs(title = glue("Weight Distrubution of Selected Gourds")) +
          xlab("weight (lbs)")
      }

    })
    
    output$boxPlot <- renderPlot({
      validate(
        need(input$gourd_type != "", "Please select a gourd.")
      )
      
      if ("All" %in% input$gourd_type) {
        gourds |> 
          ggplot(aes(x=Gourd_Type, y = weight_lbs)) + 
          geom_boxplot() + 
          labs(title = "Weight Distrubution of All Gourds") +
          xlab("weight (lbs)")
      }
      else{
        gourds |> 
          filter(Gourd_Type %in% c(input$gourd_type)) |> 
          ggplot(aes(x=Gourd_Type, y=weight_lbs)) + 
          geom_boxplot() + 
          labs(title = glue("Weight Distrubution of Selected Gourds")) +
          xlab("weight (lbs)")
      }
    })
    
    output$linePlot <- renderPlot({
      validate(
        need(input$gourd_type != "", "Please select a gourd.")
      )
      
      if ("All" %in% input$gourd_type) {
        gourds_summary |> 
          ggplot(aes(x=year_bk, color = Gourd_Type, y = max_weight)) + 
          geom_line() + 
          labs(title = "Heaviest Gourd By Year") +
          xlab("year")
      }
      else{
        gourds_summary |> 
          filter(Gourd_Type %in% c(input$gourd_type)) |> 
          ggplot(aes(x=year_bk, color = Gourd_Type, y = max_weight)) + 
          geom_line() + 
          labs(title = glue("Heaviest Gourd By Year")) +
          xlab("year")
      }
    })
    
    output$map <- renderLeaflet({
      leaflet(
        geo_WI_gourds |> 
          filter(Gourd_Type %in% c(input$gourd_type)) #need to figure out how to get the All button to work
        ) |> 
        addTiles() |>
        addCircleMarkers(
          ~long, ~lat,
          popup = ~paste("Location:", location, "<br>", 
                         "Gourd: ", Gourd_Type, "<br>", 
                         "Weight: ", weight_lbs, "lbs", "<br>", 
                         "Place: ", place, "<br>",
                         "Year :", year_bk),
          radius = ~place_weight, #1795 place max
          color = ~color,
          fillColor = ~color,
          opacity = .5
        ) |> 
        addLegend(
          "bottomright",
          colors = unname(gourd_colors),
          labels = names(gourd_colors),
          title = "Gourd Types",
          opacity = 1
        )
    })
    
    observeEvent(input$reset_input, {
      shinyjs::reset("side-panel")
    })

}
