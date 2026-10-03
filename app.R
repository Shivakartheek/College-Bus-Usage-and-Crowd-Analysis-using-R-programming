# ============================================================
# app.R
# College Bus Usage and Crowd Analysis — Shiny Dashboard
#
# Interactive companion to scripts/02_analysis.R. Reads the same
# college_bus.csv, does the same cleaning, and exposes route /
# bus / day-type / departure filters that drive every chart,
# KPI, and table live.
#
# Run with:
#   shiny::runApp("shiny_app")
# ============================================================

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

# ------------------------------------------------------------
# 1. LOAD & CLEAN (same logic as scripts/02_analysis.R)
# ------------------------------------------------------------
raw <- read.csv("college_bus.csv", stringsAsFactors = FALSE)

clean_data <- function(df) {
  df <- distinct(df)
  df$Date <- as.Date(df$Date)

  df <- df %>%
    group_by(Route) %>%
    mutate(
      Passenger_Count = ifelse(is.na(Passenger_Count),
                                round(median(Passenger_Count, na.rm = TRUE)),
                                Passenger_Count),
      Delay_Minutes = ifelse(is.na(Delay_Minutes),
                              round(median(Delay_Minutes, na.rm = TRUE)),
                              Delay_Minutes)
    ) %>%
    ungroup()

  df <- df %>%
    mutate(Occupancy_Pct = round((Passenger_Count / Bus_Capacity) * 100, 1))

  df$Day <- factor(df$Day, levels = c("Monday","Tuesday","Wednesday",
                                       "Thursday","Friday","Saturday","Sunday"))
  df$Is_Weekend <- df$Day %in% c("Saturday", "Sunday")
  df$Month <- format(df$Date, "%Y-%m")
  df
}

data_all <- clean_data(raw)

routes_all <- sort(unique(data_all$Route))
buses_all  <- sort(unique(data_all$Bus_ID))
deps_all   <- sort(unique(data_all$Departure_Time))
date_min   <- min(data_all$Date)
date_max   <- max(data_all$Date)

theme_bus <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# ------------------------------------------------------------
# 2. UI
# ------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML("
    body { background-color:#F4F6F5; }
    .well { background:#FFFFFF; border:1px solid #E1E6E4; }
    .kpi-box {
      background:#FFFFFF; border:1px solid #E1E6E4; border-radius:8px;
      padding:14px 16px; text-align:center; height:100%;
    }
    .kpi-box .val { font-size:26px; font-weight:700; color:#146C6B; }
    .kpi-box .val.alert { color:#C0392B; }
    .kpi-box .lab { font-size:12px; color:#5C6B67; margin-top:4px; }
    h4.section-title { margin-top:6px; font-weight:700; }
  "))),

  titlePanel(
    div(
      h2("🚌 College Bus Fleet Dashboard", style = "margin-bottom:0;"),
      h5("College Bus Usage and Crowd Analysis — Shiny Dashboard",
         style = "color:#5C6B67; font-weight:400;")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters", class = "section-title"),
      checkboxGroupInput("routes", "Route",
                          choices = routes_all, selected = routes_all),
      selectInput("day_type", "Service day",
                  choices = c("All days" = "all", "Weekday" = "weekday", "Weekend" = "weekend"),
                  selected = "all"),
      selectInput("dep_time", "Departure",
                  choices = c("Both" = "all", setNames(deps_all, deps_all)),
                  selected = "all"),
      dateRangeInput("date_range", "Date range",
                      start = date_min, end = date_max,
                      min = date_min, max = date_max),
      sliderInput("occ_threshold", "Overcrowded threshold (Occupancy %)",
                   min = 50, max = 150, value = 90, step = 5),
      actionButton("reset_btn", "Reset filters", icon = icon("rotate-left"))
    ),

    mainPanel(
      width = 9,

      fluidRow(
        column(2, div(class = "kpi-box", div(class = "val", textOutput("kpi_trips")), div(class = "lab", "Trips in view"))),
        column(2, div(class = "kpi-box", div(class = "val", textOutput("kpi_occ")), div(class = "lab", "Avg occupancy"))),
        column(3, div(class = "kpi-box", div(class = "val alert", textOutput("kpi_over")), div(class = "lab", "Overcrowded trips"))),
        column(2, div(class = "kpi-box", div(class = "val", textOutput("kpi_route")), div(class = "lab", "Busiest route"))),
        column(3, div(class = "kpi-box", div(class = "val", textOutput("kpi_delay")), div(class = "lab", "Avg delay (min)")))
      ),
      br(),

      tabsetPanel(
        tabPanel("Route & Bus",
          br(),
          fluidRow(
            column(6, plotOutput("plot_route", height = 320)),
            column(6, plotOutput("plot_bus", height = 320))
          )
        ),
        tabPanel("Trends",
          br(),
          fluidRow(
            column(12, plotlyOutput("plot_daily", height = 320))
          ),
          br(),
          fluidRow(
            column(6, plotOutput("plot_weekday", height = 300)),
            column(6, plotOutput("plot_month", height = 300))
          )
        ),
        tabPanel("Heatmap",
          br(),
          plotOutput("plot_heatmap", height = 380)
        ),
        tabPanel("Overcrowded Trips",
          br(),
          DTOutput("table_overcrowded")
        ),
        tabPanel("Route Summary Table",
          br(),
          DTOutput("table_routes")
        )
      )
    )
  )
)

# ------------------------------------------------------------
# 3. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$reset_btn, {
    updateCheckboxGroupInput(session, "routes", selected = routes_all)
    updateSelectInput(session, "day_type", selected = "all")
    updateSelectInput(session, "dep_time", selected = "all")
    updateDateRangeInput(session, "date_range", start = date_min, end = date_max)
    updateSliderInput(session, "occ_threshold", value = 90)
  })

  filtered <- reactive({
    df <- data_all %>% filter(Route %in% input$routes)

    if (input$day_type == "weekday") df <- df %>% filter(!Is_Weekend)
    if (input$day_type == "weekend") df <- df %>% filter(Is_Weekend)
    if (input$dep_time != "all")     df <- df %>% filter(Departure_Time == input$dep_time)

    df <- df %>% filter(Date >= input$date_range[1], Date <= input$date_range[2])
    df
  })

  # ---- KPIs ----
  output$kpi_trips <- renderText({ nrow(filtered()) })

  output$kpi_occ <- renderText({
    df <- filtered()
    if (nrow(df) == 0) return("—")
    paste0(round(mean(df$Occupancy_Pct), 1), "%")
  })

  output$kpi_over <- renderText({
    df <- filtered()
    if (nrow(df) == 0) return("—")
    n_over <- sum(df$Occupancy_Pct > input$occ_threshold)
    paste0(n_over, " (", round(100 * n_over / nrow(df), 1), "%)")
  })

  output$kpi_route <- renderText({
    df <- filtered()
    if (nrow(df) == 0) return("—")
    df %>% group_by(Route) %>% summarise(m = mean(Passenger_Count), .groups = "drop") %>%
      arrange(desc(m)) %>% slice(1) %>% pull(Route)
  })

  output$kpi_delay <- renderText({
    df <- filtered()
    if (nrow(df) == 0) return("—")
    round(mean(df$Delay_Minutes), 1)
  })

  # ---- Route summary (reused by chart + table) ----
  route_summary_r <- reactive({
    filtered() %>%
      group_by(Route) %>%
      summarise(
        Average_Passengers = round(mean(Passenger_Count), 1),
        Maximum_Passengers = max(Passenger_Count),
        Average_Occupancy_Pct = round(mean(Occupancy_Pct), 1),
        Average_Delay = round(mean(Delay_Minutes), 1),
        Trips = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Average_Passengers))
  })

  bus_summary_r <- reactive({
    filtered() %>%
      group_by(Bus_ID) %>%
      summarise(
        Average_Occupancy_Pct = round(mean(Occupancy_Pct), 1),
        Trips = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Average_Occupancy_Pct))
  })

  # ---- Charts ----
  output$plot_route <- renderPlot({
    req(nrow(filtered()) > 0)
    ggplot(route_summary_r(), aes(x = reorder(Route, -Average_Passengers), y = Average_Passengers)) +
      geom_col(fill = "#146C6B") +
      labs(title = "Average Passengers by Route", x = "Route", y = "Average Passengers") +
      theme_bus
  })

  output$plot_bus <- renderPlot({
    req(nrow(filtered()) > 0)
    bs <- bus_summary_r()
    bs$Crowded <- ifelse(bs$Average_Occupancy_Pct > input$occ_threshold, "Over threshold", "Under threshold")
    ggplot(bs, aes(x = reorder(Bus_ID, -Average_Occupancy_Pct), y = Average_Occupancy_Pct, fill = Crowded)) +
      geom_col() +
      geom_hline(yintercept = input$occ_threshold, linetype = "dashed", color = "#C0392B") +
      scale_fill_manual(values = c("Over threshold" = "#C0392B", "Under threshold" = "#146C6B")) +
      labs(title = "Average Occupancy % by Bus", x = "Bus ID", y = "Average Occupancy %", fill = "") +
      theme_bus + theme(legend.position = "top")
  })

  output$plot_daily <- renderPlotly({
    req(nrow(filtered()) > 0)
    dt <- filtered() %>% group_by(Date) %>% summarise(Total_Passengers = sum(Passenger_Count), .groups = "drop")
    p <- ggplot(dt, aes(x = Date, y = Total_Passengers)) +
      geom_line(color = "#146C6B") +
      labs(title = "Daily Passenger Trend", x = "Date", y = "Total Passengers") +
      theme_bus
    ggplotly(p) %>% layout(hovermode = "x unified")
  })

  output$plot_weekday <- renderPlot({
    req(nrow(filtered()) > 0)
    wd <- filtered() %>% filter(!Is_Weekend) %>%
      group_by(Day) %>% summarise(Average_Passengers = round(mean(Passenger_Count), 1), .groups = "drop")
    ggplot(wd, aes(x = Day, y = Average_Passengers, group = 1)) +
      geom_line(color = "#E7A100", linewidth = 1) +
      geom_point(color = "#E7A100", size = 2.5) +
      labs(title = "Average Passengers by Weekday", x = "Day", y = "Average Passengers") +
      theme_bus
  })

  output$plot_month <- renderPlot({
    req(nrow(filtered()) > 0)
    mt <- filtered() %>% group_by(Month) %>% summarise(Total_Passengers = sum(Passenger_Count), .groups = "drop")
    ggplot(mt, aes(x = Month, y = Total_Passengers)) +
      geom_col(fill = "#E7A100") +
      labs(title = "Monthly Passenger Totals", x = "Month", y = "Total Passengers") +
      theme_bus
  })

  output$plot_heatmap <- renderPlot({
    req(nrow(filtered()) > 0)
    hm <- filtered() %>%
      group_by(Route, Departure_Time) %>%
      summarise(Average_Passengers = mean(Passenger_Count), .groups = "drop")
    ggplot(hm, aes(x = Departure_Time, y = Route, fill = Average_Passengers)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#EBF8F7", high = "#146C6B") +
      labs(title = "Route vs Departure Time Heatmap", x = "Departure Time", y = "Route",
           fill = "Avg\nPassengers") +
      theme_bus
  })

  # ---- Tables ----
  output$table_overcrowded <- renderDT({
    df <- filtered() %>%
      filter(Occupancy_Pct > input$occ_threshold) %>%
      arrange(desc(Occupancy_Pct)) %>%
      select(Date, Day, Bus_ID, Route, Departure_Time, Passenger_Count, Bus_Capacity, Occupancy_Pct, Delay_Minutes)
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })

  output$table_routes <- renderDT({
    datatable(route_summary_r(), options = list(pageLength = 10), rownames = FALSE)
  })
}

# ------------------------------------------------------------
# 4. RUN
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)
