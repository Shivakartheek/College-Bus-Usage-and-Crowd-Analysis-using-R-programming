# ============================================================
# 02_analysis.R
# College Bus Usage and Crowd Analysis Using R
#
# Answers:
#   1. Which route has the highest passenger count
#   2. Peak travel timings
#   3. Most crowded buses
#   4. Average passengers per route
#   5. Weekday vs weekend usage
#   6. Overcrowded trips
#   7. Low-utilization routes
#   8. Monthly passenger trends
#
# Produces 7 charts (saved as PNG in plots/) + printed summaries.
# ============================================================

library(dplyr)
library(ggplot2)

dir.create("plots", showWarnings = FALSE)

# ------------------------------------------------------------
# 1. LOAD & INSPECT
# ------------------------------------------------------------
data <- read.csv("data/college_bus.csv", stringsAsFactors = FALSE)

cat("\n================ RAW DATA SUMMARY ================\n")
str(data)
summary(data)
cat("Rows:", nrow(data), " | Duplicate rows:", sum(duplicated(data)), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(data)))

# ------------------------------------------------------------
# 2. DATA CLEANING
# ------------------------------------------------------------
data <- distinct(data)                       # drop exact duplicates
data$Date <- as.Date(data$Date)

# Impute missing Passenger_Count / Delay_Minutes with the route-wise
# median, a defensible choice for count-like operational data.
data <- data %>%
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

# Derived column: Occupancy %
data <- data %>%
  mutate(Occupancy_Pct = round((Passenger_Count / Bus_Capacity) * 100, 1))

data$Day <- factor(data$Day, levels = c("Monday","Tuesday","Wednesday",
                                         "Thursday","Friday","Saturday","Sunday"))
data$Is_Weekend <- data$Day %in% c("Saturday", "Sunday")
data$Month <- format(data$Date, "%Y-%m")
data$Departure_Hour <- substr(data$Departure_Time, 1, 2)

cat("\nCleaning complete. Rows after cleaning:", nrow(data), "\n")

# ------------------------------------------------------------
# 3. ROUTE-LEVEL SUMMARY  (Q1, Q4)
# ------------------------------------------------------------
route_summary <- data %>%
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

cat("\n================ ROUTE SUMMARY ================\n")
print(route_summary)

top_route <- route_summary$Route[1]
cat("\n>>", top_route, "has the highest average passenger count",
    "(", route_summary$Average_Passengers[1], "avg /",
    route_summary$Maximum_Passengers[1], "max ).\n")

# ------------------------------------------------------------
# 4. PEAK TRAVEL TIMINGS  (Q2)
# ------------------------------------------------------------
time_summary <- data %>%
  group_by(Departure_Time) %>%
  summarise(
    Total_Passengers = sum(Passenger_Count),
    Average_Passengers = round(mean(Passenger_Count), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Passengers))

cat("\n================ PEAK TRAVEL TIMES ================\n")
print(time_summary)
cat("\n>> Peak travel period:", time_summary$Departure_Time[1], "\n")

# ------------------------------------------------------------
# 5. MOST CROWDED BUSES  (Q3)
# ------------------------------------------------------------
bus_summary <- data %>%
  group_by(Bus_ID) %>%
  summarise(
    Average_Occupancy_Pct = round(mean(Occupancy_Pct), 1),
    Trips_Over_90pct = sum(Occupancy_Pct > 90),
    Total_Trips = n(),
    Pct_Trips_Over_90 = round(100 * Trips_Over_90pct / Total_Trips, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_Occupancy_Pct))

cat("\n================ MOST CROWDED BUSES ================\n")
print(head(bus_summary, 5))
cat("\n>>", bus_summary$Bus_ID[1], "is the most consistently crowded bus (",
    bus_summary$Average_Occupancy_Pct[1], "% avg occupancy,",
    bus_summary$Pct_Trips_Over_90[1], "% of trips over 90% ).\n")

# ------------------------------------------------------------
# 6. WEEKDAY vs WEEKEND USAGE  (Q5)
# ------------------------------------------------------------
weekday_weekend <- data %>%
  group_by(Is_Weekend) %>%
  summarise(
    Average_Passengers = round(mean(Passenger_Count), 1),
    Total_Passengers = sum(Passenger_Count),
    Trips = n(),
    .groups = "drop"
  ) %>%
  mutate(Period = ifelse(Is_Weekend, "Weekend", "Weekday"))

cat("\n================ WEEKDAY vs WEEKEND ================\n")
print(weekday_weekend)

# ------------------------------------------------------------
# 7. OVERCROWDED TRIPS  (Q6)
# ------------------------------------------------------------
OVERCROWD_THRESHOLD <- 90  # % occupancy

overcrowded_trips <- data %>%
  filter(Occupancy_Pct > OVERCROWD_THRESHOLD) %>%
  arrange(desc(Occupancy_Pct))

cat("\n================ OVERCROWDED TRIPS (>", OVERCROWD_THRESHOLD, "% ) ================\n")
cat("Total overcrowded trips:", nrow(overcrowded_trips),
    "(", round(100 * nrow(overcrowded_trips) / nrow(data), 1), "% of all trips )\n")
print(head(overcrowded_trips[, c("Date","Bus_ID","Route","Passenger_Count",
                                  "Bus_Capacity","Occupancy_Pct")], 10))

# ------------------------------------------------------------
# 8. LOW-UTILIZATION ROUTES  (Q7)
# ------------------------------------------------------------
LOW_UTIL_THRESHOLD <- 50  # % occupancy

low_util_routes <- route_summary %>%
  filter(Average_Occupancy_Pct < LOW_UTIL_THRESHOLD) %>%
  arrange(Average_Occupancy_Pct)

cat("\n================ LOW-UTILIZATION ROUTES (<", LOW_UTIL_THRESHOLD, "% avg occupancy) ================\n")
print(low_util_routes)

# ------------------------------------------------------------
# 9. MONTHLY PASSENGER TRENDS  (Q8)
# ------------------------------------------------------------
monthly_trend <- data %>%
  group_by(Month) %>%
  summarise(
    Total_Passengers = sum(Passenger_Count),
    Average_Passengers = round(mean(Passenger_Count), 1),
    .groups = "drop"
  ) %>%
  arrange(Month)

cat("\n================ MONTHLY TRENDS ================\n")
print(monthly_trend)

# ------------------------------------------------------------
# 10. CORRELATION CHECK (Delay vs Occupancy)
# ------------------------------------------------------------
corr_val <- cor(data$Occupancy_Pct, data$Delay_Minutes)
cat("\nCorrelation between Occupancy % and Delay Minutes:", round(corr_val, 3), "\n")

# ============================================================
# CHARTS
# ============================================================

theme_bus <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# 1. Passengers by route
p1 <- ggplot(route_summary, aes(x = reorder(Route, -Average_Passengers),
                                 y = Average_Passengers)) +
  geom_col(fill = "#2b6cb0") +
  labs(title = "Average Passengers by Bus Route", x = "Route", y = "Average Passengers") +
  theme_bus
ggsave("plots/01_passengers_by_route.png", p1, width = 7, height = 5)

# 2. Passenger trend by date
daily_trend <- data %>%
  group_by(Date) %>%
  summarise(Total_Passengers = sum(Passenger_Count), .groups = "drop")

p2 <- ggplot(daily_trend, aes(x = Date, y = Total_Passengers)) +
  geom_line(color = "#2b6cb0", linewidth = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "#e53e3e", linewidth = 0.8) +
  labs(title = "Daily Passenger Trend", x = "Date", y = "Total Passengers") +
  theme_bus
ggsave("plots/02_passenger_trend_by_date.png", p2, width = 9, height = 5)

# 3. Average delay by route
p3 <- ggplot(route_summary, aes(x = reorder(Route, -Average_Delay), y = Average_Delay)) +
  geom_col(fill = "#dd6b20") +
  labs(title = "Average Delay by Route", x = "Route", y = "Average Delay (minutes)") +
  theme_bus
ggsave("plots/03_average_delay_by_route.png", p3, width = 7, height = 5)

# 4. Bus occupancy percentage
p4 <- ggplot(bus_summary, aes(x = reorder(Bus_ID, -Average_Occupancy_Pct),
                               y = Average_Occupancy_Pct)) +
  geom_col(fill = "#38a169") +
  geom_hline(yintercept = OVERCROWD_THRESHOLD, linetype = "dashed", color = "#e53e3e") +
  labs(title = "Average Occupancy % by Bus", x = "Bus ID", y = "Average Occupancy %") +
  theme_bus
ggsave("plots/04_bus_occupancy_percentage.png", p4, width = 7, height = 5)

# 5. Route vs time heatmap
heatmap_data <- data %>%
  group_by(Route, Departure_Time) %>%
  summarise(Average_Passengers = mean(Passenger_Count), .groups = "drop")

p5 <- ggplot(heatmap_data, aes(x = Departure_Time, y = Route, fill = Average_Passengers)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#ebf8ff", high = "#2b6cb0") +
  labs(title = "Route vs Departure Time Heatmap", x = "Departure Time",
       y = "Route", fill = "Avg\nPassengers") +
  theme_bus
ggsave("plots/05_route_time_heatmap.png", p5, width = 7, height = 5)

# 6. Most crowded buses
p6 <- ggplot(head(bus_summary, 6), aes(x = reorder(Bus_ID, -Average_Occupancy_Pct),
                                        y = Average_Occupancy_Pct)) +
  geom_col(fill = "#805ad5") +
  labs(title = "Most Crowded Buses (Top 6 by Avg Occupancy %)",
       x = "Bus ID", y = "Average Occupancy %") +
  theme_bus
ggsave("plots/06_most_crowded_buses.png", p6, width = 7, height = 5)

# 7. Weekday passenger trend
weekday_trend <- data %>%
  filter(!Is_Weekend) %>%
  group_by(Day) %>%
  summarise(Average_Passengers = round(mean(Passenger_Count), 1), .groups = "drop")

p7 <- ggplot(weekday_trend, aes(x = Day, y = Average_Passengers, group = 1)) +
  geom_line(color = "#2b6cb0", linewidth = 1) +
  geom_point(color = "#2b6cb0", size = 2.5) +
  labs(title = "Average Passengers by Weekday", x = "Day", y = "Average Passengers") +
  theme_bus
ggsave("plots/07_weekday_passenger_trend.png", p7, width = 7, height = 5)

cat("\nAll 7 charts saved to plots/\n")
cat("\n================ ANALYSIS COMPLETE ================\n")
