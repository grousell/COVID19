
# Script to load and clean data for COVID-19 app --------------------------
# Libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(leaflet)
library(lubridate)
library(gghighlight)
library(vroom)
library(sf)
library(leaflet)
library(leaflet.minicharts)
library(readxl)
library(DT)
library(shinyWidgets)

# Load Data ---------------------------------------------------------------

load_data <- function (){
  library(vroom)
  library(tidyverse)
  library(lubridate)
  # test_df <<- vroom ("ind_data_TEST.csv") 
  # ind_data <<- vroom ("ind_data_TEST.csv") %>% 
  #   janitor::clean_names()
  # agg_data <<- vroom ("agg_data_TEST.csv") %>% 
  #   janitor::clean_names() 
  vacc_data_hosp_icu <<- read_csv("https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/274b819c-5d69-4539-a4db-f2950794138c/download/vac_status_hosp_icu.csv") %>% 
    janitor::clean_names()
  vacc_data <<- read_csv ("https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/eed63cf2-83dd-4598-b337-b288c0a89a16/download/cases_by_vac_status.csv") %>% 
    janitor::clean_names()
  
  ind_data <<- read_csv ("https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv") %>%
    janitor::clean_names()
  agg_data <<- read_csv ("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/ed270bb8-340b-41f9-a7c6-e8ef587e6d11/download/covidtesting.csv") %>%
    janitor::clean_names()

}

load_data()

current_as_of <- tail (agg_data$reported_date, 1)
current_date <- now()
current_time <- purrr::map_chr(
  str_split(now(), " "), 2
)

## Individual Data ---- 
ind_data <- ind_data %>%
  janitor::clean_names() %>%
  select (row_id,
          accurate_episode_date,
          outcome1,
          reporting_phu_id,
          reporting_phu) %>%
  mutate (accurate_episode_date = ymd (accurate_episode_date)) %>%
  arrange (reporting_phu)


## Aggregate Data ---- 

agg_data <- agg_data %>% 
  janitor::clean_names() %>% 
  select (reported_date,
          total_cases,
          n_test = total_tests_completed_in_the_last_day,
          pos_rate = percent_positive_tests_in_last_day,
          confirmed_positive,
          resolved,
          deaths,
          in_hospital = number_of_patients_hospitalized_with_covid_19,
          in_icu = number_of_patients_in_icu_due_to_covid_19,
          on_vent = number_of_patients_in_icu_on_a_ventilator_due_to_covid_19,
          total_alpha = total_lineage_b_1_1_7_alpha,
          total_beta = total_lineage_b_1_351_beta, 
          total_gamma = total_lineage_p_1_gamma, 
          total_delta = total_lineage_b_1_617_2_delta) %>% 
  mutate (reported_date = ymd(reported_date))

## Vaccination Data ----

vacc_status_levels <- c("Unvaccinated",
                        "Partially Vaccinated",
                        "Vaccinated")

vacc_data_all <- vacc_data %>% 
  select (Date = date, 
          `Unvaccinated` = covid19_cases_unvac, 
          `Partially Vaccinated` = covid19_cases_partial_vac, 
          Vaccinated =  covid19_cases_full_vac,
          Unknown = covid19_cases_vac_unknown) %>%
  #mutate (Date = mdy (Date)) %>% 
  pivot_longer(-Date,
               values_to = "N",
               names_to = "Status") %>% 
  mutate (N = ifelse (is.na (N), 0, N)) %>% 
  group_by(Date) %>% 
  mutate (Percent = N / sum (N, na.rm = TRUE)) %>% 
  mutate (Status = parse_factor (Status,
                                 levels = c("Unknown", vacc_status_levels )
                                 ),
          group = "All Cases"
          )

  
vacc_data_hosp <- vacc_data_hosp_icu %>% 
  select (Date = date, 
          `Unvaccinated` = hospitalnonicu_unvac, 
          `Partially Vaccinated` = hospitalnonicu_partial_vac, 
          Vaccinated =  hospitalnonicu_full_vac) %>% 
  pivot_longer(-Date,
               values_to = "N",
               names_to = "Status") %>% 
  mutate (N = ifelse (is.na (N), 0, N)) %>% 
  group_by(Date) %>% 
  mutate (Percent = N / sum (N, na.rm = TRUE)) %>% 
  mutate (Status = parse_factor (Status,
                                 levels = c("Unknown", vacc_status_levels)
                                 ),
          group = "Cases in Hospital")


vacc_data_icu <- vacc_data_hosp_icu %>% 
  select (Date = date, 
          `Unvaccinated` = icu_unvac, 
          `Partially Vaccinated` = icu_partial_vac, 
          Vaccinated =  icu_full_vac) %>% 
  pivot_longer(-Date,
               values_to = "N",
               names_to = "Status") %>% 
  mutate (N = ifelse (is.na (N), 0, N)) %>% 
  group_by(Date) %>% 
  mutate (Percent = N / sum (N, na.rm = TRUE)) %>% 
  mutate (Status = parse_factor(Status, 
                                levels = vacc_status_levels
                                ),
          group = "Cases in ICU"
  ) 

vacc_data_agg <- rbind (vacc_data_all, 
                        vacc_data_hosp, 
                        vacc_data_icu)  

remove (vacc_data_all, 
        vacc_data_hosp, 
        vacc_data_icu)

## School Data ---- 


school_covid_all <- vroom ("https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/dc5c8788-792f-4f91-a400-036cdf28cfe8/download/schoolrecentcovid2021_2022.csv", 
                           col_types = cols (.default = "c")
)

# Data Wrangle for Plot 1  -----------------------------------------------------------------

## aggregate data ----

long_data <- agg_data %>% 
  select("Reported Date" = reported_date, 
         "Cumulative Total Cases" = total_cases,
         "Current Active Cases" = confirmed_positive,
         "Resolved Cases" = resolved,
         "Cumulative Total Deaths" = deaths,
         "Number of Tests Completes" = n_test, 
         "Positivity Rate" = pos_rate,
         "In Hopsital" = in_hospital,
         "In ICU" = in_icu,
         "On Ventilator" = on_vent
         ) %>% 
  pivot_longer(-`Reported Date`,
               names_to = "group",
               values_to = "n") 


## Plot 1 - Facets ------------------------------------------------------------

long_data_R <- reactive(long_data)

plot_1_function <- function(df){
  ggplot (df, 
          aes (x = `Reported Date`, 
               y = n, 
               group = group)) + 
    geom_line (aes (color = group)) + 
    labs (title = "Status of COVID-19 in Ontario",
          x = "",
          y = "") + 
    theme_minimal() + 
    scale_x_date(date_breaks = "months" , 
                 date_labels = "%b-%y") + 
    facet_wrap (~group, scales = "free", ncol = 3
    )  + 
    theme (legend.position = "none",
           axis.text.x = element_text(angle = 90, size = 8),
           panel.spacing.y = unit(1, "lines")
    )
  }

plot_1 <- plot_1_function(long_data)


# New Daily Cases ----

## Key Dates ----
today <- Sys.Date()
two_week_lag <- today - 14
rolling_mean <- tibbletime::rollify(mean, window = 7) # Function to calculate 7 day average
shutdown <- ymd(as.Date("2021-04-03"))
phase_1 <- ymd(as.Date("2021-06-11"))
phase_2 <- ymd(as.Date("2021-06-30"))
phase_3 <- ymd(as.Date("2021-07-16"))
school_reopen <- ymd(as.Date("2021-09-07"))

## Aggregate cases ----
new_cases_df <- agg_data %>% 
  arrange (reported_date) %>% 
  dplyr::filter (reported_date > "2020-02-28") %>% 
  mutate(new_cases = total_cases - dplyr::lag(total_cases),
         Time = ifelse (reported_date > two_week_lag, "Previous 14 Days", paste0 ("Data up to ", two_week_lag))) %>%
  mutate (Time = parse_factor(Time),
          `7 Day Average` = rolling_mean(new_cases)) 

ymax <- max (new_cases_df$new_cases, na.rm = TRUE) + 50

seven_day_max <- round (max (new_cases_df$`7 Day Average`, na.rm = TRUE), 1)

date_new_cases_current <- max(new_cases_df$reported_date)
n_new_cases_current <- new_cases_df %>% 
  dplyr::filter (reported_date == date_new_cases_current) %>% 
  select (new_cases) %>% 
  pull()

n_new_cases_prev_day <- new_cases_df %>% 
  dplyr::filter (reported_date == date_new_cases_current - 1) %>% 
  select (new_cases) %>% 
  pull()

change_new_cases <- n_new_cases_current - n_new_cases_prev_day

change_new_cases <- ifelse (change_new_cases > 0, paste0("(Up ", change_new_cases, ")"),
                            ifelse (change_new_cases < 0, paste0("(Down ", abs(change_new_cases), ")"),
                                    "(No Change)")
                            )


# Plot ----

new_cases_plot_function <- function (df){
  df %>% 
    ggplot(aes (x = reported_date, y = new_cases)) + 
    geom_bar (aes(y = new_cases, fill = Time), stat = "identity") +
    geom_line (aes (y = `7 Day Average`, 
                    group = 1, 
                    color = "7 Day Average")) + 
    scale_fill_manual(name = "", values = c("grey60", "#0B6EA1")) +
    scale_color_manual(name = "", values = c("black")) +
    scale_x_date(expand = c(0,7),
                 date_breaks = "1 month",
                 date_labels =  "%b %d") +
    scale_y_continuous(limits = c(0, ymax)) + 
    # 7 Day Average ----
    geom_hline(yintercept = seven_day_max, size = 0.25) +
    annotate( "text", 
              size = 3,  
              x = ymd(as.Date("2020-06-15")),  
              y = seven_day_max + 225, 
              label =paste0 ("Highest 7 Day Average\n", seven_day_max)) + 
    # Shutdown ----
    geom_segment(
      aes(x = shutdown, y = 0, xend = shutdown, yend = 3500), size = 0.25
                 ) +
    annotate(
      "text", size = 3, x = shutdown - 10, y = 4000, label = "Provincial\nShutdown"
      ) +
    # Phase 1 ----
    geom_segment(
      aes(x = phase_1, y = 0, xend = phase_1, yend = 2000), size = 0.25
                 ) +
    annotate(
      "text", size = 3, x = phase_1, y = 2100, label = "Reopening\nPhase 1"
      ) +
    # Phase 2 ----
    geom_segment(
      aes(x = phase_2, y = 0, xend = phase_2, yend = 1200), size = 0.25
      ) +
    annotate(
      "text", size = 3, x = phase_2, y = 1300, label = "Reopening\nPhase 2"
      ) +
    # Phase 3 ----
    geom_segment(
      aes(x = phase_3, y = 0, xend = phase_3, yend = 400), size = 0.25
      ) +
    annotate("text", size = 3, x = phase_3 - 1, y = 500, label = "Reopening\nPhase 3"
             ) +
    geom_segment(
      aes(x = school_reopen, y = 0, xend = school_reopen, yend = 1000), size = 0.25
      ) +
    annotate("text", size = 3, x = school_reopen - 1, y = 1100, label = "Schools\nReopening"
             ) +
    # Labels ----
    labs (title = "Number of New COVID-19 Cases by Date",
          x = "", 
          y = "New Cases") + 
    theme (panel.background = element_rect(fill = "NA"),
           legend.title = element_blank()
    ) 
}


# Cases by Vaccination Status ---------------------------------------------


# vacc_data_agg %>% 
#   dplyr::filter (group == "Cases in ICU") %>%  
#   ggplot (aes (x = Date, y = value, fill = fct_rev(name))) +
#   geom_bar(position = "stack", stat = "identity") + 
#   facet_wrap(~group, ncol = 1, scales = "free_y") + 
#   scale_fill_manual(values = RColorBrewer::brewer.pal(4, "Paired")) + 
#   geom_text (aes (label = value),
#              position = position_stack(vjust = 0.5),
#              size = 3) + 
#   scale_x_date(date_breaks = "days" , 
#                date_labels = "%y-%m-%d") + 
#   labs (title = "COVID-19 Cases in Ontario",
#         subtitle = "By Vaccinaton Status",
#         x = "Date",
#         y = "Number of Cases")+
#   theme_minimal() +
#   theme (legend.title = element_blank(),
#          axis.text.x = element_text(angle = 90, size = 8),
#          legend.position = "bottom"
#   )
#   



# Schools Map -------------------------------------------------------------
## Set up data ----
school_covid <- read_csv ("https://raw.githubusercontent.com/grousell/COVID19/main/Data/school_covid.csv",
                          col_types = cols(collected_date = col_character(),
                                           reported_date = col_character(),
                                           school_board = col_character(),
                                           school = col_character(),
                                           municipality = col_character(),
                                           confirmed_student_cases = col_double(),
                                           confirmed_staff_cases = col_double(),
                                           total_confirmed_cases = col_double(),
                                           confirmed_unspecified_cases = col_double(),
                                           lat = col_double(),
                                           lon = col_double()
                          ),
                          locale = readr::locale(encoding = "UTF-8")
) %>%
  rename (Students = confirmed_student_cases,
          Staff = confirmed_staff_cases,
          Unspecified = confirmed_unspecified_cases) 


n_students <- sum (school_covid$Students, na.rm = TRUE)
n_staff <- sum (school_covid$Staff, na.rm = TRUE)
n_unspecified <- sum (school_covid$Unspecified, na.rm = TRUE)
n_total <- sum (school_covid$total_confirmed_cases, na.rm = TRUE)
n_schools <- length (school_covid$school)
n_school_boards <- length(unique(school_covid$school_board))

## Create Map ----

map_school <- leaflet(school_covid) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(
    school_covid$lon, school_covid$lat,
    type = "pie",
    chartdata = school_covid[, c("Students", 
                                 "Staff",
                                 "Unspecified")],
    colorPalette = c("green", "purple", "blue"),
    showLabels = TRUE,
    layerId = paste0(school_covid$school, " - ", school_covid$school_board),
    labelText = paste0(school_covid$school, " - ", school_covid$school_board), 
    width = school_covid$total_confirmed_cases * 5
  )

## List of Schools ----

list_schools <- datatable(school_covid %>% 
                            select (`Report Date` = reported_date,
                                    `School Board` = school_board, 
                                    School = school,
                                    Students,
                                    Staff, 
                                    Unspecified,
                                    Total = total_confirmed_cases), 
                          options = list(pageLength = 40, 
                                         scrollX = TRUE,
                                         scrollY = '400px',
                                         autoWidth = FALSE),
                          rownames = FALSE)
## School cases over time ----
palette <- c("#C5E8DD", "#FDC3AC", "#8DA0CB", "#F6D3E8")

school_over_time <- school_covid_all %>% 
  mutate (reported_date = ymd (reported_date)) %>% 
  group_by (reported_date) %>%
  summarise (Student = sum (as.numeric(confirmed_student_cases)),
             Staff = sum (as.numeric(confirmed_staff_cases)), 
             Unspecified = sum (as.numeric (confirmed_unspecified_cases))
  ) %>% 
  pivot_longer(cols = c (Student, Staff, Unspecified)) %>% 
  rename (Date = reported_date, 
          Group = name, 
          N = value)

school_over_time_totals <- school_over_time %>% 
  group_by (Date) %>% 
  summarise (Total = sum (N, na.rm = TRUE)) 

school_over_time_plot <- school_over_time %>% 
  ggplot (aes (x = Date, y = N, fill = Group)) + 
  geom_col() + 
  scale_fill_manual(values = palette) + 
  geom_text (aes (label = N), 
             position = position_stack(vjust = 0.5), 
             size = 1.5
  ) + 
  geom_text (aes (x = Date, y = Total + 20, label = Total, fill = NULL),
             data = school_over_time_totals,
             size = 2
  ) +
  labs (title = "Active School Cases of COVID-19",
        subtitle = "Over Time",
        x = "Reported Date", 
        y = "Number of Cases") + 
  theme(legend.title=element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(fill="NA")
  )

school_over_time_plot <- ggplotly (school_over_time_plot)
## PHU Plots
### Totals ----
phu_aggregates <- ind_data %>%
  group_by (accurate_episode_date , reporting_phu) %>%
  count() %>%
  # dplyr::filter (reporting_phu == "Hamilton Public Health Services" ) %>%
  arrange (accurate_episode_date) %>%
  ungroup() %>%
  mutate (total_n = cumsum(n),
          accurate_episode_date = ymd (accurate_episode_date)) %>% 
  rename (`Accurate Episode Date` = accurate_episode_date,
          `Number of Cases` = total_n)

phu_total_cases <- function (df){
  ggplot (df, aes (x = `Accurate Episode Date`, 
                  y = `Number of Cases`, 
                  colour = reporting_phu)) + 
    geom_line(colour = "#0B6EA1") +
    scale_x_date(expand = c(0,7),
                 date_breaks = "2 weeks",
                 date_labels =  "%b %d") +
    labs ("Total Number of COVID-19 Cases",
          x = "",
          y = "Number of Cases") +
    theme (panel.background = element_rect(fill = "NA"),
           legend.title = element_blank(),
           plot.title = element_text(hjust = 0.5),
           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
}

# phu_total_cases(phu_aggregates)
### New Daily Cases ----

phu_daily_cases_table <- function(df){
  df %>% 
    dplyr::filter (accurate_episode_date >= "2020-01-01") %>%
    #dplyr::filter(reporting_phu == "Hamilton Public Health Services") %>%
    group_by (accurate_episode_date) %>%
    count() %>%
    rename(Date = accurate_episode_date, 
           `New Cases` = n) %>% 
    mutate (Date = as.Date(Date)) %>% 
    complete(Date = seq.Date(as.Date("2020-01-01"), max(Date), by="day")) %>% 
    replace_na(list(`New Cases` = 0)) %>% 
    distinct() %>% 
    ungroup() %>% 
    mutate (`7 Day Average` = rolling_mean(`New Cases`)) %>%
    mutate (Time = ifelse (Date > two_week_lag, 
                           "Previous 14 Days", 
                           paste0 ("Data up to ", two_week_lag)))
}

phu_daily_cases_plot <- function (df){
  new_cases_max <- max(df$`New Cases`)
  plot <- df %>% 
    ggplot (aes (x = Date, y = `New Cases`)) +
    geom_bar (stat = "identity") + 
    geom_bar (aes(y = `New Cases`, fill = Time), stat = "identity") +
    geom_line (aes (y = `7 Day Average`, group = 1, color = "7 Day Average")) + 
    scale_fill_manual(name = "", values = c("grey60", "#0B6EA1")) +
    scale_color_manual(name = "", values = c("black")) +
    scale_x_date(expand = c(0,7),
                 date_breaks = "2 weeks",
                 date_labels =  "%b %d") +
    scale_y_continuous(limits = c(0, new_cases_max)) +
    # Shutdown -----------------------------------
  geom_segment(aes(x = shutdown,
                   y = 0,
                   xend = shutdown,
                   yend = new_cases_max * 0.9)
  ) +
    annotate("text",
             size = 3,
             x = shutdown - 10,
             y = new_cases_max * 0.95,
             label = "Provincial\nShutdown") +
    # Phase 1 -----------------------------------
  geom_segment(aes(x = phase_1,
                   y = 0,
                   xend = phase_1,
                   yend = new_cases_max * 0.75)
  ) +
    annotate("text",
             size = 3,
             x = phase_1,
             y = new_cases_max * 0.80,
             label = "Reopening Phase 1") +
    # Phase 2 -----------------------------------
  geom_segment(aes(x = phase_2,
                   y = 0,
                   xend = phase_2,
                   yend = new_cases_max * 0.65)
  ) +
    annotate("text",
             size = 3,
             x = phase_2,
             y = new_cases_max *0.70,
             label = "Reopening Phase 2") +
    # Phase 3 -----------------------------------
  geom_segment(aes(x = phase_3,
                   y = 0,
                   xend = phase_3,
                   yend = new_cases_max * 0.55)
  ) +
    annotate("text",
             size = 3,
             x = phase_3,
             y = new_cases_max * 0.6,
             label = "Reopening Phase 3") +
    # Labels --------------------------
  labs (title = "New COVID-19 Cases by Date",
        x = "", 
        y = "Number of New Cases") + 
    # Theme --------------------------
  theme (panel.background = element_rect(fill = "NA"),
         legend.title = element_blank(),
         plot.title = element_text(hjust = 0.5),
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )
  return (plot)
}

test <- phu_daily_cases_table(ind_data %>% 
                                dplyr::filter(reporting_phu == "Hamilton Public Health Services")) 

# phu_daily_cases_plot(test)
# Text for Webpage --------------------------------------------------------
## ABout ----
about <- "
<h3>About the Data User Group</h3><br>
<h3>Who are we?</h3>
We are a group of researchers and analysts who are interested in data science and would like to use our expertise to contribute to the understanding of COVID-19 in our communities.<br> 

<h3>Looking for data</h3>

One of the challenges we encountered trying to understand the spread of COVID-19 was finding a data source in a format that is easily accessible for analysis. When we were unable to locate such a file (and finding that the process to scrape data through R was too messy given the formats that the information has been released) we decided to take a manual approach. Using a few different sources, we have compiled data tables which are easily accessible in R (our favorite) and Python.
Opening the data from the source. <br>

A 
<a href='https://docs.google.com/spreadsheets/d/1Y3_qYkTJK6Vnhw3RCOhOiwafm4-PQxd5l0Hxk4x1ZxE/edit'>googlesheet</a> has been created and is being maintained using data from an Ontario government website and resources available on two Wikipedia pages. We will continue to update these tables until a more authoritative source of case records is made available, ideally by Public Health Ontario.

You can also access the data directly from the <a href= 'https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/ed270bb8-340b-41f9-a7c6-e8ef587e6d11/download/covidtesting.csv'> ministry’s webpage page.</a><br>

<h3>Resources: An invitation to explore and dive deeper</h3>

As we explore this data we will be sharing visualizations and insights on the Data User Group website. Our hope is that others will find our summaries useful. We extend an open invitation to others interested in data science to engage in additional analysis and use this data set for your own exploration
Data background and sources

You can read more about the project at the Data User Group website.

"

## New Cases ----

new_cases <- "
One of the key indicators in determining when to ease restrictions in Ontario is the number of new cases. There needs to be a <a href='https://www.ontario.ca/page/framework-reopening-our-province#section-2'>consistent two to four week decrease in the number of new daily COVID 19 cases
"

## Vaccine status ----

vacc_status_text <- "
The charts here show the number and percentage of COVID-19 cases broken down by vaccination status. 
"
## Schools ---- 

schools <- "
This map shows the school boards with reported cases of COVID-19 as reported to the Ministry of Health. The dataset can be found <a href='https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/8b6d22e2-7065-4b0f-966f-02640be366f2/download/schoolsactivecovid.csv'> here</a>. The Data User Group has started and maintained a data sheet with the schools' geocode <a href=https://raw.githubusercontent.com/DUGroup/Ontario-COVID-19-Research/master/Data/schools_geocode.csv'> here</a>.
<br>
<br>
The circle shows where confirmed cases have occurred with either a staff member or student. Clicking the circle will show the number of confirmed cases for both staff and students.  
<br>
<br>
"
schools_current_as_of <- paste0 ("Data is current as of ", school_covid$reported_date[1])
## PHUs ----

phu_text <- "
The data is based on the <a href='https://data.ontario.ca/en/dataset/confirmed-positive-cases-of-covid-19-in-ontario'> provincial data set </a> and reflects the last submission by the Public Health Unit.  
<br>
<br>
"