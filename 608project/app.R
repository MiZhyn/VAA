#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/


# library(shiny)
library(shiny)
library(ggiraph)
library(dplyr)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(shinyWidgets)
library(readr)

merged_df <- read_rds("merged_df.rds")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .box {
        border-radius: 10px;
        box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
        transition: 0.3s;
        background-color: #f8f9fa;
        margin-bottom: 20px;
      }
      .box:hover {
        box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
      }
    "))
  ),
  titlePanel("Cargo Match and Legality Status"),
  
  # User input options in a single row
  fluidRow(
    column(12,
           div(style = "display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 20px;",
               pickerInput("quarter", "Quarter:", 
                           choices = unique(merged_df$quarter),
                           multiple = TRUE,
                           selected = unique(merged_df$quarter),
                           options = list(`actions-box` = TRUE, style = "width: 200px; display: inline-block")),
               pickerInput("fish", "Fish Species:", 
                           choices = unique(merged_df$fish),
                           multiple = TRUE,
                           selected = unique(merged_df$fish),
                           options = list(`actions-box` = TRUE, style = "width: 200px; display: inline-block")),
               pickerInput("port", "Ports:", 
                           choices = unique(merged_df$city),
                           multiple = TRUE,
                           selected = unique(merged_df$city),
                           options = list(`actions-box` = TRUE, style = "width: 200px; display: inline-block"))
           )
    )
  ),
  
  # Scatter plot in the first column, pie chart and bar chart in the second column
  fluidRow(
    column(8,
           div(class = "box", girafeOutput("plot", height = "600px"))
    ),
    column(4,
           div(class = "box", plotlyOutput("illegal_by_port", height = "300px")),
           div(class = "box", plotlyOutput("illegal_by_fish", height = "300px"))
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    merged_df %>%
      filter(quarter %in% input$quarter,
             fish %in% input$fish,
             city %in% input$port)
  })
  
  output$plot <- renderGirafe({
    p <- ggplot(filtered_data(), aes(x = quarter, y = 0, 
                                     fill = status,
                                     shape = illegal,
                                     data_id = cargo,
                                     tooltip = paste("Date:", date_added, "<br>",
                                                     "Source Transaction:", cargo, "<br>",
                                                     "Probable Vessel:", probable_vessel, "<br>",
                                                     "Match Status:", unmatch, "<br>",
                                                     "Legal Status:", illegal))) +
      geom_point_interactive(size = 4, alpha = 0.8, 
                             position = position_jitter(width = 0.45, height = 0.2)) +
      scale_fill_manual(values = c("match.Legal" = "#3498db", 
                                   "match.Illegal" = "#e74c3c",
                                   "unmatch.Legal" = "#95a5a6",
                                   "unmatch.Illegal" = "#e67e22"),
                        labels = c("Match & Legal", "Match & Illegal",
                                   "Unmatch & Legal", "Unmatch & Illegal")) +
      scale_shape_manual(values = c("Legal" = 21, "Illegal" = 24),
                         labels = c("Legal", "Illegal")) +
      labs(title = "Cargo Match and Legality Status by Quarter and City",
           x = "Transaction Quarter", y = NULL, fill = "Status", shape = "Legality") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 12),
        panel.grid.major.x = element_line(color = "#ecf0f1"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 10),
        strip.text.y = element_text(size = 12, angle = 0),
        legend.position = "top",
        strip.background = element_rect(fill = "#bdc3c7", color = "#7f8c8d"),
        panel.background = element_rect(fill = "#f8f9fa"),
        plot.background = element_rect(fill = "#f8f9fa",color = NA), 
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
      ) +
      facet_grid(target ~ ., scales = "free", space = "free") +
      guides(x = guide_axis(angle = 0),
             fill = guide_legend(override.aes = list(shape = c(21, 24, 21, 24), size = 5)), 
             shape = guide_legend(override.aes = list(shape = c(21, 24), fill = "black", size = 5)))
    
    girafe(ggobj = p, width_svg = 20, height_svg = 12.5,
           options = list(
             opts_hover(css = "fill:black;stroke:black;r:5pt;"),
             opts_selection(type = "single", css = "fill:red;stroke:red;r:5pt;")
           ))
  })
  
  output$illegal_by_port <- renderPlotly({
    illegal_port_data <- filtered_data() %>%
      group_by(city) %>%
      summarise(
        total = n(),
        illegal = sum(illegal == "Illegal"),
        legal = total - illegal
      ) %>%
      arrange(desc(illegal))
    
    plot_ly(illegal_port_data, labels = ~city, values = ~illegal, type = "pie",
            textposition = "inside",
            textinfo = "label+percent",
            insidetextfont = list(color = '#FFFFFF', size = 14),
            hoverinfo = "text",
            text = ~paste(city, "<br>Illegal Orders:", illegal, 
                          "<br>Total Orders:", total),
            marker = list(colors = colorRampPalette(c("#3498db", "#e74c3c"))(nrow(illegal_port_data)),
                          line = list(color = '#FFFFFF', width = 1.5))) %>%
      layout(title = list(text = "Illegal Order Ratio by Port", font = list(size = 16, face = "bold")),
             showlegend = FALSE,
             hoverlabel = list(bgcolor = "white", font = list(size = 14)),
             paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa",
             margin = list(t = 50, b = 50, l = 50, r = 50))
  })
  
  output$illegal_by_fish <- renderPlotly({
    illegal_fish_data <- filtered_data() %>%
      filter(illegal == "Illegal") %>%
      group_by(fish) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count)) %>%
      arrange(desc(percentage))
    
    plot_ly(illegal_fish_data, x = ~fish, y = ~percentage, type = "bar",
            marker = list(color = "#e74c3c", line = list(color = "#c0392b", width = 1.5)),
            hoverinfo = "text",
            text = ~scales::percent(percentage)) %>%
      layout(title = list(text = "Fish Species in Illegal Orders", font = list(size = 16, face = "bold")),
             xaxis = list(title = "Fish Species", tickangle = 0, tickfont = list(size = 12)),
             yaxis = list(title = "Percentage", tickformat = ".0%", tickfont = list(size = 12)),
             hoverlabel = list(bgcolor = "white", font = list(size = 14)),
             paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa",
             margin = list(t = 50, b = 100, l = 50, r = 50))
  })
}

shinyApp(ui = ui, server = server)