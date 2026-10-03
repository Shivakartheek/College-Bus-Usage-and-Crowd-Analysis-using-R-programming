# ============================================================
# 01_generate_data.R
# Generates a realistic synthetic college bus dataset for the
# "College Bus Usage and Crowd Analysis Using R" project.
#
# Simulates 90 days of operation across 6 routes and 10 buses,
# 2 trips/day/bus (morning + evening), with realistic patterns:
#   - Weekday ridership >> weekend ridership
#   - Morning (8 AM) and evening (4:30 PM) peaks
#   - A few routes that run consistently near/over capacity
#   - A couple of low-utilization routes
#   - Occasional delays, correlated loosely with occupancy
# ============================================================

set.seed(42)

# ---- Configuration -------------------------------------------------
start_date <- as.Date("2025-01-01")
n_days     <- 90
dates      <- seq(start_date, by = "day", length.out = n_days)

routes <- data.frame(
  Route         = paste0("Route ", 1:6),
  Distance_KM   = c(12, 18, 9, 25, 14, 7),
  # base popularity multiplier per route (drives avg passengers)
  Popularity    = c(1.15, 0.95, 1.30, 0.60, 1.00, 0.55),
  stringsAsFactors = FALSE
)

buses <- data.frame(
  Bus_ID       = sprintf("B%02d", 1:10),
  Bus_Capacity = c(50, 50, 45, 60, 50, 45, 50, 60, 45, 50),
  stringsAsFactors = FALSE
)

# Each bus is assigned to one primary route for the semester
bus_route_map <- data.frame(
  Bus_ID = buses$Bus_ID,
  Route  = rep(routes$Route, length.out = nrow(buses)),
  stringsAsFactors = FALSE
)

trip_slots <- data.frame(
  Departure_Time = c("08:00", "16:30"),
  Trip            = c("Morning", "Evening"),
  stringsAsFactors = FALSE
)

# ---- Build trip grid -------------------------------------------------
grid <- merge(data.frame(Date = dates), bus_route_map)
grid <- merge(grid, trip_slots)
grid <- merge(grid, routes, by = "Route")
grid <- merge(grid, buses,  by = "Bus_ID")

grid$Day <- weekdays(grid$Date)
grid$Is_Weekend <- grid$Day %in% c("Saturday", "Sunday")

# Drop most weekend trips (colleges run a reduced skeleton service)
keep_weekend <- runif(nrow(grid)) < 0.15
grid <- grid[!grid$Is_Weekend | keep_weekend, ]

# ---- Simulate Passenger_Count -----------------------------------------
n <- nrow(grid)

# base load as a fraction of capacity, driven by route popularity
base_fraction <- pmin(pmax(rnorm(n, mean = 0.65, sd = 0.15) * grid$Popularity, 0.1), 1.25)

# weekend trips carry far fewer passengers
base_fraction[grid$Is_Weekend] <- base_fraction[grid$Is_Weekend] * runif(sum(grid$Is_Weekend), 0.2, 0.4)

# morning trips slightly busier than evening (everyone goes in together,
# leaves more staggered)
base_fraction[grid$Trip == "Morning"] <- base_fraction[grid$Trip == "Morning"] * 1.08

# mild month-over-month trend: ridership dips slightly mid-semester,
# builds again toward exams
month_index <- as.numeric(format(grid$Date, "%m")) - as.numeric(format(start_date, "%m"))
seasonal_factor <- 1 + 0.06 * sin(month_index / 2)
base_fraction <- base_fraction * seasonal_factor

grid$Passenger_Count <- round(pmin(base_fraction, 1.30) * grid$Bus_Capacity)
grid$Passenger_Count <- pmax(grid$Passenger_Count, 3)  # never near-empty

# ---- Simulate Delay_Minutes --------------------------------------------
occ_pct_raw <- grid$Passenger_Count / grid$Bus_Capacity
grid$Delay_Minutes <- pmax(0, round(
  rnorm(n, mean = 3, sd = 2) + pmax(occ_pct_raw - 0.85, 0) * 25
))

# ---- Build Arrival_Time from Departure_Time + travel time -------------
speed_kmph <- 22  # average city/college-route speed including stops
travel_minutes <- round((grid$Distance_KM / speed_kmph) * 60) + grid$Delay_Minutes

dep_dt <- as.POSIXct(paste(grid$Date, grid$Departure_Time), format = "%Y-%m-%d %H:%M", tz = "UTC")
arr_dt <- dep_dt + travel_minutes * 60
grid$Arrival_Time <- format(arr_dt, "%H:%M")

# ---- Final column selection & ordering ---------------------------------
bus_data <- grid[, c("Date", "Day", "Bus_ID", "Route", "Departure_Time",
                      "Arrival_Time", "Passenger_Count", "Bus_Capacity",
                      "Distance_KM", "Delay_Minutes")]

bus_data <- bus_data[order(bus_data$Date, bus_data$Departure_Time, bus_data$Bus_ID), ]
rownames(bus_data) <- NULL

# Introduce a few realistic messy rows for the "data cleaning" step
# (a handful of NAs, one duplicate row) -- comment out if you want a
# perfectly clean dataset instead.
na_rows <- sample(nrow(bus_data), 6)
bus_data$Delay_Minutes[na_rows[1:3]] <- NA
bus_data$Passenger_Count[na_rows[4:6]] <- NA
bus_data <- rbind(bus_data, bus_data[sample(nrow(bus_data), 1), ])  # duplicate

write.csv(bus_data, "data/college_bus.csv", row.names = FALSE)

cat("Generated", nrow(bus_data), "trip records ->  data/college_bus.csv\n")

