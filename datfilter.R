

# crashdata ----
# Load data
crashData <- read.csv("./data/LowerMainlandCrashes_FullData_data.csv")

# Group data by intersection and summarize crash count
groupedData <- crashData %>%
    group_by(Location, Longitude, Latitude) %>%
    summarize(totalCrashes = sum(Crash.Count))

#years separated 
yearsData <- crashData %>%
    group_by(Year, Location, Longitude, Latitude)%>%
    summarise(totalyrcount = n())

# Define Vancouver coordinates
vancouver <- c(49.25, -123.1207)

# Define the style of the CircleMarkers
markerStyle <- function(count) {
    col <- ifelse(count >= 600, "black",
                  ifelse(count >= 200, "red",
                         ifelse(count >= 100,"blue","orange" )))
    list(radius = count/60, fillOpacity = 0.7, fillColor = col, color = NA)
}

# Signal data ----

signals <- read_excel("./data/traffic-signals.xlsx")
signals <- signals %>% separate(geo_point_2d, c("lat", "lon"), sep=",", remove=F, convert = TRUE)
lat <- as.numeric(stringr::str_extract(signals$geo_point_2d, "\\d+\\.\\d+"))
lng <- as.numeric(stringr::str_extract(signals$geo_point_2d, "-\\d+\\.\\d+"))

labels <- sprintf("<strong>%s</strong><br/>Click for more info",signals$TYPE) %>% lapply(htmltools::HTML)

greater250 <- groupedData%>%
    filter(totalCrashes >= 250)

value = c(5, 10, 15)



# Amir's data-------

percapitacrimes <- read_csv("./data/percapitacrimes.csv")

#PLOT1
# read the data
crim <- read.csv("./data/crimedata_csv_AllNeighbourhoods_AllYears.csv")

# filter data
theft_from_vehicle <- crim %>%
    filter(TYPE == "Theft from Vehicle") %>%
    select(TYPE, NEIGHBOURHOOD)

# visualize count in terms of neighborhood
neighborhood_counts <- theft_from_vehicle %>%
    group_by(NEIGHBOURHOOD) %>%
    summarise(count = n())

# create the bar plot
ggplot_obj <- ggplot(neighborhood_counts, aes(x = count, y = reorder(NEIGHBOURHOOD, count))) +
    geom_bar(stat = "identity", fill = "#e31a1c", width = 0.8) +
    scale_fill_gradient2(low = "#f1eef6", mid = "#fdae61", high = "#e31a1c", midpoint = max(neighborhood_counts$count)/2, na.value = NA) +
    labs(x = "Count", y = "Neighborhood") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray90"),
          panel.grid.minor.x = element_line(color = "gray90"),
          panel.border = element_blank(),
          panel.background = element_blank())

#plot2
# create a data frame with the cities and percentages
city_data <- data.frame(
    City = c("Burnaby", "Coquitlam", "Delta", "Langley City", 
             "Langley Township", "Maple Ridge", "New Westminster", "North Vancouver",
             "North Vancouver District", "Pitt Meadows", "Port Coquitlam", 
             "Port Moody", "Richmond", "Surrey", "Vancouver", "West Vancouver", 
             "White Rock", "Electoral Area A"),
    percentage = c(16.51, 3.91, 4.35, 1.23, 4.94, 2.00, 2.55, 2.42, 2.58, 
                   0.49, 1.96, 0.80, 11.14, 14.39, 35.41, 1.36, 0.55, 1.82)
)
#plot 3

dataV <- data.frame(
    Factor = c("Aggressive Driving", "Distracted Driving/Inattention", "Driver Error/Confusion",
               "Environmental", "Impairment", "Medical Issue", "Road Issue", "Speeding",
               "Vehicle Issue", "Wild Animal"),
    `2010` = c(56, 102, 100, 60, 127, 40, 9, 113, 18, 7),
    `2011` = c(50, 79, 60, 61, 75, 16, 9, 98, 10, 4),
    `2012` = c(40, 80, 46, 72, 57, 16, 10, 100, 18, 2),
    `2013` = c(45, 77, 56, 47, 64, 15, 7, 77, 14, 3),
    `2014` = c(30, 66, 54, 77, 65, 32, 7, 81, 12, 2),
    `2015` = c(43, 89, 63, 67, 72, 18, 11, 89, 9, 2),
    `2016` = c(51, 80, 56, 53, 67, 17, 13, 92, 14, 2),
    `2017` = c(49, 73, 58, 54, 72, 23, 5, 73, 13, 6),
    `2018` = c(73, 58, 54, 72, 23, 5, 73, 13, 6, 4),
    `2019` = c(33, 70, 54, 52, 58, 25, 13, 74, 7, 3)
)

#vtk
# Create a data frame with the VKT per capita values and city names
VKT_pc <- data.frame(
    Cities = c("Maple Ridge", "Langley Township", "White Rock", "Delta", "Pitt Meadows",
               "Port Moody", "Langley City", "Port Coquitlam", "Surrey", "North Vancouver District",
               "Coquitlam", "West Vancouver", "New Westminster", "North Vancouver City",
               "Richmond", "Burnaby", "Vancouver", "University Endowment Lands (UBC)"),
    vkt_per_capita = c(33.1, 31.2, 29.8, 25.6, 24.3, 24.0, 23.8, 22.9, 21.5, 21.0, 20.6,
                       17.2, 15.9, 15.5, 14.6, 14.4, 10.7, 10.1)
)

# Reorder the rows based on vkt_per_capita
VKT_pc <- VKT_pc %>% 
    arrange(desc(vkt_per_capita))

# Create a custom color palette based on the city names
city_colors <- c("#FF5733", "#FFC300", "#3D9970", "#39CCCC", "#001f3f", "#7FDBFF", "#F012BE",
                 "#B10DC9", "#111111", "#2ECC40", "#FF851B", "#85144b", "#7FDBFF", "#FF4136",
                 "#001f3f", "#0074D9", "#F012BE", "#39CCCC")




