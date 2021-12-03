libs <- c("tidyverse", "fastverse", "here", "lubridate", "openair", "glue", "janitor", "viridis", "ggExtra", "openairmaps")
library(xfun)
pkg_attach2(libs)

source("../airquality_GIT/importODS.R")
# install.packages("remotes")
# remotes::install_github("davidcarslaw/openairmaps")

# period <- " IN ['2021-11-01T00:00:00' TO '2021-11-07T23:59:00']"
date_on <- "2021-11-13"
date_off <- "2021-11-22"
sensor_id <- "66987"

sts_sensors_vec <- 
c("66963", 
"66966",
"66970",
"66972",
"66974",
"66979",
"66987",
"67568",
"67655",
"67665")

field_filter_str_fnc <- function(field_name = "siteid", values_vec = c("203", "215")){

    field_assign <- str_glue("{field_name} = ")
    field_collapse = str_glue(" OR {field_assign}")

pasted_str <- paste0(values_vec, collapse = field_collapse)
ods_search_str <- str_glue("{field_assign}{pasted_str}")
return(ods_search_str)

}
# paste into search box on portal
sts_sensors <- field_filter_str_fnc(field_name = "sensor_id",
                                    values_vec = sts_sensors_vec)

ld_raw <- getODSExport(select_str = "date, pm10, pm2_5",
                       date_col = "date",
                       dateon = date_on,
                       dateoff = date_off,
                       where_str = glue("sensor_id = {66987}"),
                       dataset = "luftdaten_pm_bristol") %>% 
    rename(pm2.5 = pm2_5)
#get st. werbs sensors
ld_all_raw_tbl <- getODSExport(select_str = "sensor_id, date, pm10, pm2_5, geo_point_2d",
                               date_col = "date",
                               dateon = date_on,
                               dateoff = date_off,
                               where_str = sts_sensors,
                               dataset = "luftdaten_pm_bristol") %>% 
    rename(pm2.5 = pm2_5)
# put into format needed for openairmaps
ld_all_tbl <- ld_all_raw_tbl %>% 
    separate(geo_point_2d,
             into = c("latitude", "longitude"),
             sep = ",",
             convert = TRUE) %>% 
    mutate(sensor_id = as_factor(sensor_id)) %>% 
    select(date, pm2.5, pm10, sensor_id, latitude, longitude) %>% 
    filter(date >= as.POSIXct(date_on, tz = "UTC"),
           date <= as.POSIXct(date_off, tz = "UTC"))

met_raw <- getODSExport(select_str = "date_time, temp, ws, wd, rh",
                        where_str = "",
                        date_col = "date_time",
                        dateon = date_on,
                        dateoff = date_off,
                        dataset = "met-data-bristol-lulsgate")

met_proc_tbl <- met_raw %>% 
    select(date = date_time, ws, wd, rh, temp) %>% 
    timeAverage(avg.time = "hour")

joined_tbl <- ld_raw %>% 
    left_join(met_proc_tbl, by = "date") %>% 
    mutate(pm10_Polarplot = list(polarPlot(., pollutant = "pm10", main = glue("PM10 Polar Plot: Sensor {sensor_id}"), k = 24, statistic = "weighted.mean")))

scatterPlot(joined_tbl, x = "pm10", y = "temp", method = "hexbin", col = "jet",
            border = "grey", xbin = 10)

scatterPlot(joined_tbl,
            x = "date",
            y = "pm2.5",
            cols = "firebrick",
            pch = 16,
            col = "red",
            alpha = 0.5,
            windflow = list(scale = 0.15, lwd = 2),
            key = TRUE,
            key.footer = "pm2.5\n (ugm-3)")

cls <- openColours(c( "darkgreen", "yellow", "red", "purple"), 10)  
# Trendlevel ----

ld_time_tbl <- ld_raw %>% 
    mutate(day = (as_date(date)),
           hour = (hour(date)),
           month = month(date, label = TRUE, abbr = FALSE),
           year = year(date))

ld_time_tbl %>% 
    trendLevel(pollutant = "pm2.5", x = "day", cols = cls)

# Heatmap ggplot2 ----

title <- expression(paste(expression("PM" [2.5] * " " * mu * "g m" ^-3 * "at sensor "), sensor_id))

p <- ld_time_tbl %>% 
    ggplot(aes(day, hour, fill = pm2.5))+
    geom_tile(color = "white", size = 0.1) + 
    # scale_fill_viridis(name = expression("PM" [2.5] * " " * mu * "g m" ^-3 * "  "),
    #                    option = "C") +
    scale_fill_gradientn(colours = cls) +
    facet_grid(year ~ month) +
    scale_y_continuous(trans = "reverse", breaks = unique(ld_time_tbl$hour)) +
    scale_x_continuous(breaks = unique(ld_time_tbl$day)) +
    theme_minimal(base_size = 8) +
    labs(title= glue("Heatmap for Sensor: {sensor_id}"),
         x = "Day",
         y = "Hour Commencing") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 6)) +
    theme(strip.background = element_rect(colour = "white")) +
    theme(plot.title = element_text(hjust = 0)) +
    theme(axis.ticks = element_blank()) +
    theme(axis.text = element_text(size = 7)) + 
    theme(legend.title = element_text(size = 10), ) +
    theme(legend.text = element_text(size = 6)) +
    removeGrid()#ggExtra


p


# Polar plot - multiple ---- 
#            pm25_plot = list(polarPlot(., pollutant = "pm2.5", main = glue("PM2.5 Polar Plot: Sensor {sensor_id}"))))



# joined_tbl <- ld_raw %>% 
#     select(date, sensor_id, pm2.5 = pm2_5, everything()) %>% 
#     left_join(met_proc_tbl, by = "date") %>% 
#     filter(sensor_id %in% c(7675, 10491)) %>% # for testing
#     nest_by(sensor_id) %>% 
#     mutate(pm10_plot = list(polarPlot(data, pollutant = "pm10", main = glue("PM10 Polar Plot: Sensor {sensor_id}"))),
#            pm25_plot = list(polarPlot(data, pollutant = "pm2.5", main = glue("PM2.5 Polar Plot: Sensor {sensor_id}"))))

# joined_tbl$pm10_plot[[2]][[1]]


# polarPlot SIngle ----
single_tbl <- ld_raw %>% 
    # select(date, sensor_id, pm2.5, everything()) %>% 
    left_join(met_proc_tbl, by = "date")

pp <- polarPlot(single_tbl,
                pollutant = "pm10",
                cols = cls,
                main = glue("Polar plot for PM10 at sensor: {sensor_id}"),
                uncertainty = FALSE)

# Calendar Plot AQ Index
labels <- c("1 - Low", "2 - Low", "3 - Low", "4 - Moderate", "5 - Moderate",
            "6 - Moderate", "7 - High", "8 - High", "9 - High", "10 - Very High")
# o3.breaks <-c(0, 34, 66, 100, 121, 141, 160, 188, 214, 240, 500)
# no2.breaks <- c(0, 67, 134, 200, 268, 335, 400, 468, 535, 600, 1000)
pm10.breaks <- c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000)
pm25.breaks <- c(0, 12, 24, 35, 42, 47, 53, 59, 65, 70, 1000)


days <- ld_time_tbl %>%
    select(day) %>% 
    n_distinct() 

if(days > 7){
    
}

pm25_calplot <- ld_time_tbl %>% 
    calendarPlot(pollutant = "pm2.5",
                 labels = labels,
                 statistic = "mean",
                 breaks = pm25.breaks,
                 annotate = "value")

pm10_calplot <- ld_time_tbl %>% 
    calendarPlot(pollutant = "pm10",
                 labels = labels,
                 statistic = "mean",
                 breaks = pm10.breaks,
                 annotate = "value",
                 cols = cls)


# Openair maps -----

ld_all_met_tbl <- ld_all_tbl %>% 
    left_join(met_proc_tbl, by = "date") %>% 
    na_omit(cols = c("ws", "wd"))

polarMap(ld_all_met_tbl,
         dir_polar = "plots/polar",
         alpha = 0.5,
         pollutant = "pm10",
         x = "ws",
         k = 20,
         latitude = "latitude",
         longitude = "longitude",
         provider = "OpenStreetMap",
         type = "sensor_id",
         cols = cls)
