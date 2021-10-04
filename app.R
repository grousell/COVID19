
source ("load_clean_data.R")


current_label <- paste0("Current as of: ", 
                        wday (current_as_of, label = TRUE), 
                        " ", 
                        month(current_as_of, label = TRUE),
                        " ", 
                        day (current_as_of),
                        "<br>"
                        )

ui <- fluidPage(
  includeCSS("www/styles.css"),
  navbarPage("Ontario COVID-19", 
             # Page 1 - About ------
             tabPanel(
               "New Daily Cases",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   htmlOutput("about")
                 ),
                 mainPanel(
                   fluidRow(
                     HTML (current_label)
                            ),
                   fluidRow(
                     infoBox(HTML("New Cases<br>", 
                                  n_new_cases_current,
                                  change_new_cases, "<br>"),
                             icon = icon ("chart-line"),
                             width = 4),
                     infoBox(HTML("Active Cases:<br>"), 
                             tail (agg_data$confirmed_positive, 1),
                             icon = icon("chart-area"),
                             width = 4
                     ),
                     infoBox(HTML("Deaths:<br>"), 
                             tail (agg_data$deaths, 1),
                             icon = icon("chart-area"),
                             width = 4
                     )
                   ),
                   fluidRow(
                     plotlyOutput("new_cases_plot")
                   )
                 )
               )
             ) ,
             
             # Page 2 - New Daily Cases -----------------------------------------------
             tabPanel (
               "Status of COVID-19 in Ontario",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   htmlOutput("new_cases")
                 ),
                 mainPanel(
                   fluidRow(
                     selectInput("plot_1_choice", 
                                 "Zoom to Metric",
                                 choices = c("All", unique(long_data$group)
                                 )
                     )
                     ),
                   fluidRow(
                     plotlyOutput("Plot1")
                   )
                 )
               )
             ),
             # Page 3 - Vaccination Status ------------------------------
             tabPanel(
               "Cases by Vaccination Status",
               sidebarLayout(
                 sidebarPanel(
                   width = 3, 
                   htmlOutput("vacc_status_text")
                   
                 ),
                 mainPanel(
                   fluidRow(
                     column(5, 
                            sliderTextInput(
                              inputId = "switch1",
                              label = "",
                              choices = c("N", "Percent"),
                              width = "75px"
                            )
                     ), 
                     column(5,
                            selectInput(
                              "vacc_select",
                              "Select Metric",
                              choices = c ("All Cases",
                                           "Cases in Hospital",
                                           "Cases in ICU" )
                            )
                     )
                   ),
                   fluidRow(
                     textOutput("test")
                   ),
                   fluidRow(
                     plotlyOutput("vacc_status")
                   )
                 )
               )
             ),
             # Page 4 - Schools  -----------------------------------------------
             tabPanel (
               "Schools",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   htmlOutput("schools")
                 ),
                 mainPanel(
                   fluidRow( ## Info Boxes ----
                             infoBox(HTML("Total Cases:<br>"), 
                                     tail (n_total, 1),
                                     icon = icon("chart-line"),
                                     width = 2
                             ),
                             infoBox(HTML("Students:<br>"), 
                                     tail (n_students, 1),
                                     icon = icon("chart-line"),
                                     width = 2
                             ),
                             infoBox(HTML("Staff:<br>"), 
                                     tail (n_staff, 1),
                                     icon = icon("chart-line"),
                                     width = 2
                             ),
                             infoBox(HTML("Unspecified:<br>"), 
                                     tail (n_unspecified, 1),
                                     icon = icon("chart-line"),
                                     width = 2
                             ),
                             infoBox(HTML("Schools:<br>"), 
                                     tail (n_schools, 1),
                                     icon = icon("chart-line"),
                                     width = 2
                             ),
                             infoBox(HTML("School Boards:<br>"), 
                                     tail (n_school_boards, 1),
                                     icon = icon("chart-line"),
                                     width = 2
                             )
                   ),
                   fluidRow(
                     htmlOutput("schools_current_as_of")
                   ),
                   div(style = "font-size: 10px; padding: 14px 0px; margin:0%",
                       fluidRow( ## Panels ----
                                 tabsetPanel(
                                   tabPanel(### Map ----
                                            "Map of Active Cases",
                                            leafletOutput("map_school", height = 600)
                                   ),
                                   tabPanel (### List of Schools ----
                                             "List of Schools",
                                             dataTableOutput("list_schools")),
                                   tabPanel(### School Cases over time ----
                                            "School Cases over Time",
                                            plotlyOutput("over_time_schools"))
                                 )
                       )
                   )
                 )
               )
             ),
             # Page 5 - Public Health Units  -----------------------------------------------
             tabPanel (
               "Public Health Units",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   htmlOutput("phu_text"),
                   selectInput("phu_choice", 
                               "Public Health Unit",
                               choices = sort(unique(ind_data$reporting_phu))
                   )
                 ),
                 mainPanel(
                   plotlyOutput("phu_plot_total"),
                   plotlyOutput("phu_plot_daily")
                 )
               )
             ) ## End PHU ----
  )
)
# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  output$test <- renderText(input$switch1)
  
  output$about <- renderText (about)
  output$new_cases <- renderText (new_cases)
  output$schools <- renderText(schools)
  output$phu_text <- renderText(phu_text)
  output$Plot1 <- renderPlotly({
    if (input$plot_1_choice == "All"){
      ggplotly(plot_1_function(long_data),
               height = 600)} else {
                 ggplotly (
                   plot_1_function(long_data %>% dplyr::filter (group == input$plot_1_choice)
                   ) ,
                   height = 600
                 )
               }
  }
  ) %>% 
    bindCache (input$plot_1_choice)
  ### New Cases ----
  output$new_cases_plot <- renderPlotly(
    ggplotly(
      new_cases_plot_function(new_cases_df)
    ) %>%
      layout(legend = list(orientation = "h",
                           x = 0.1,
                           y = -0.1)
      )
  )
  
  ### Vacc Status ----
  output$vacc_status_text <- renderText(vacc_status_text)
  output$vacc_status <- renderPlotly({
    if (input$switch1 == "N"){
      ggplotly (
        vacc_data_agg %>% 
          mutate (Status = fct_rev(Status)) %>% 
          dplyr::filter (group == input$vacc_select) %>%  
          ggplot (aes (x = Date, y = N, fill = Status)
          ) +
          geom_bar(position = "stack", stat = "identity") + 
          facet_wrap(~group, ncol = 1, scales = "free_y") +
          scale_fill_manual(values = RColorBrewer::brewer.pal(4, "Paired")) + 
          geom_text (aes (label = N),
                     position = position_stack(vjust = 0.5),
                     size = 2) + 
          scale_x_date(date_breaks = "weeks" , 
                       date_labels = "%y-%m-%d") +
          theme_minimal() +
          theme (legend.title = element_blank(),
                 axis.text.x = element_text(angle = 90, size = 8),
                 legend.position = "bottom"
          )
      )
      
    } else{
      ggplotly (vacc_data_agg %>% 
                  mutate (Status = fct_rev(Status)) %>% 
                  dplyr::filter (group == input$vacc_select) %>%  
                  ggplot (aes (x = Date, y = Percent, fill = Status)
                  ) +
                  geom_bar(position = "stack", stat = "identity") + 
                  facet_wrap(~group, ncol = 1, scales = "free_y") +
                  scale_fill_manual(values = RColorBrewer::brewer.pal(4, "Paired")) + 
                  geom_text (aes (label = scales::percent(Percent, accuracy = 1)),
                             position = position_stack(vjust = 0.5),
                             size = 2) + 
                  scale_x_date(date_breaks = "weeks" , 
                               date_labels = "%y-%m-%d") +
                  scale_y_continuous(labels = scales::percent_format()) + 
                  theme_minimal() +
                  theme (legend.title = element_blank(),
                         axis.text.x = element_text(angle = 90, size = 8),
                         legend.position = "bottom"
                  )
      )
    }
  })
  
  
  ### Schools ----
  output$map_school <- renderLeaflet(map_school)
  output$list_schools <- renderDataTable(list_schools)
  output$over_time_schools <- renderPlotly(school_over_time_plot)
  
  ### PHUs ----
  output$phu_plot_total <- renderPlotly(ggplotly (phu_total_cases (phu_aggregates %>% 
                                                                     dplyr::filter (reporting_phu == input$phu_choice)
  ),
  tooltip = c("Accurate Episode Date", "Number of Cases")
  )
  ) %>% 
    bindCache (input$phu_choice)
  output$phu_plot_daily <- renderPlotly(
    ggplotly(phu_daily_cases_plot(
      phu_daily_cases_table(
        ind_data %>% dplyr::filter (reporting_phu == input$phu_choice)
      )
    ), tooltip = c("Date", "New Cases", "7 Day Average")
    )
  ) %>% 
    bindCache (input$phu_choice)
  
}

# Run the application ----
shinyApp(ui = ui, server = server)

