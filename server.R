# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Work PC uses: rsconnect 0.4.3 

# server ----
shinyServer(function(input, output, session) {
    
    # Navbar ------------------------------------------------------------------
    shinyjs::addClass(id = "navBar", class = "navbar-right")
    
    # Tab 2 (AllYears) -----------------------
  
    
    # Create a leaflet map centered on Vancouver
    output$myMap <- renderLeaflet({
        leaflet() %>% 
            setView(lng = vancouver[2], lat = vancouver[1], zoom = 11.5) %>% 
            addTiles(group = "OSM") %>%
            addCircleMarkers(data = crashData, lng = ~Longitude, lat = ~Latitude,
                             popup = paste("Location: ", crashData$Location,
                                           "<br>Crash count: ", crashData$Crash.Count,
                                           "<br>Year: ", crashData$Year),
                             label = ~as.character(Crash.Count),
                             clusterOptions = markerClusterOptions()) %>%
            addProviderTiles("CartoDB", group = "Carto") %>%
            addLayersControl(baseGroups = c("OSM", "CartoDB"))
    })
    
    output$mySecondMap <- renderLeaflet({
        leaflet() %>% 
            setView(lng = vancouver[2], lat = vancouver[1], zoom = 11.5) %>% 
            addTiles(group = "OSM") %>%
            addCircleMarkers(data = groupedData, lng = ~Longitude, lat = ~Latitude,
                             fillColor = ~markerStyle(totalCrashes)$fillColor,
                             radius = ~markerStyle(totalCrashes)$radius,
                             fillOpacity = ~markerStyle(totalCrashes)$fillOpacity,
                             color = ~markerStyle(totalCrashes)$color,
                             popup = paste("Location: ", groupedData$Location,
                                           "<br>Crash count: ", groupedData$totalCrashes),
                             label = ~as.character(totalCrashes))%>%
            # Add legend
            addLegend(position = "bottomright",
                      title = "Crash count",
                      colors = c("orange", "blue","red", "black"),
                      labels = c("<= 99 ", "100-199", "200-599", " >= 600"),
                      opacity = 1) %>%
            addProviderTiles("CartoDB", group = "Carto") %>%
            addLayersControl(baseGroups = c("OSM", "CartoDB"))
        
    })
    # Tab 2 (others) ----
    # Create a reactive subset of the data based on the selected year
    filteredData <- reactive({
        subset(yearsData, Year == input$year)
    })
    
    # Create a leaflet map centered on Vancouver with the filtered data
    output$thirdMap <- renderLeaflet({
        leaflet() %>% 
            setView(lng = vancouver[2], lat = vancouver[1], zoom = 11.5) %>% 
            addTiles() %>% 
            addHeatmap(data =filteredData(), lng = ~Longitude, lat = ~Latitude,
                             gradient = c("yellow", "orange", "red", "darkred"),
                             intensity = ~value,
                             blur = 20,
                             radius = 10)%>%
                             addLayersControl(baseGroups = c("OSM", "CartoDB", "Esri")) %>%
            addProviderTiles("CartoDB", group = "Carto") %>%
            addProviderTiles("Esri", group = "Esri") 
    }) 
    
    # Tab 3 (signals)---------
             
    filteredSignals <- reactive({
        signals %>%
            filter(TYPE %in% input$signal_types)
        
     
    })
    
  
    # Create the Leaflet map
    output$signal_map <- renderLeaflet({
        # Set initial view of map
        leaflet(data = filteredSignals()) %>%
            setView(lng = vancouver[2], lat = vancouver[1], zoom = 11.5) %>%
            # Add tiles
            addTiles() %>%
            # Add circle markers
            addCircleMarkers(
                lng = ~lon,
                lat = ~lat,
                label = labels,
                fillColor = ~ifelse(
                    TYPE == "Pedestrian Actuated Signal", "blue",
                    ifelse(TYPE == "Semi Actuated", "orange",
                           ifelse(TYPE == "Fixed Time", "green",
                           ifelse(TYPE == "RRFB", "red",
                           ifelse(TYPE == "Fully Actuated", "purple",
                           ifelse(TYPE == "Special Crosswalk", "yellow",
                           ifelse(TYPE == "Bus Actuated Signal", "brown",
                           ifelse(TYPE == "FH", "gray",
                           ifelse(TYPE == "CS", "black", "white"))))))))
                    ),
                    radius = 5,
                    fillOpacity = 0.8,
                    stroke = FALSE,
                    popup = paste("Type: ", signals$TYPE)
                )%>%  
        addHeatmap(data =crashData, lng = ~Longitude, lat = ~Latitude,
                   gradient = c("yellow", "orange", "red", "darkred"),
                   intensity = ~value,
                   blur = 20,
                   radius = 10)%>%
        addLayersControl(baseGroups = c("OSM", "CartoDB", "Esri"))
    

    })#close

    
    #Tab 4 (Crime)---------
    
    output$plot1 <- renderPlotly({
        ggplotly(ggplot_obj, tooltip = c("count")) %>%
            layout(title = NULL)
    })
        # Create the bar chart
    output$barplot <- renderPlotly({
      b <- ggplot(percapitacrimes, aes(x = Neighbourhood, y = `Per Capita Crime`, fill = Neighbourhood)) +
        guides(fill = FALSE) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        labs(x = "Neighbourhood", y = "Per Capita Crime")
      ggplotly(b, tooltip = c("x", "y")) %>%
        layout(title = NULL)
    })    
    
    output$plot2 <- renderPlotly({
        # create a horizontal bar plot with ggplot2
        p <- ggplot(city_data, aes(x = percentage, y = reorder(City, percentage), fill = City)) +
            geom_bar(stat = "identity") +
            scale_fill_hue(l = 40) +
            labs(x = "Percentage of Jobs", y = NULL) +
            theme_minimal() +
            theme(legend.key.size = unit(0.5, "cm"))
        
        # convert ggplot to plotly
        ggplotly(p, tooltip = c("y", "x"))
    })
    
    output$plot3 <- renderPlot({
        dataV2 <- dataV %>% 
            pivot_longer(cols = -Factor, names_to = "Year", values_to = "Value")
        
        filtered_data <- dataV2 %>% filter(Factor == input$factor)
       
            ggplot(filtered_data, aes(x = Year, y = Value)) +
                geom_col(fill = "blue") +
                labs(x = "Year", y = "Number of Fatalities") +
                ggtitle("Contributing Factors to Fatalities: Bar chart")
        
    })
    output$vkt_bar <- renderPlotly({
        # Create the plot
        p1 <- ggplot(VKT_pc, aes(x = vkt_per_capita, y = reorder(Cities, vkt_per_capita), fill = Cities)) +
            geom_bar(stat = "identity") +
            xlab("VKT per capita") +
            ylab("") +
            scale_fill_manual(values = city_colors) +
            theme(plot.title = element_text(size = 8, hjust = 0.5),
                  legend.text = element_text(size = 6),
                  legend.key.size = unit(0.3, "cm"))
        
        # Make the plot interactive
        ggplotly(p1, tooltip = c("vkt_per_capita")) %>%
            layout(hovermode = "closest")
    })
    
    # Tab 5 (scrap) ---------
    
    
    if (!dir.exists("www")) {
      dir.create("www")
    }
    

    # Read in the text file and perform sentiment analysis
    file_data <- reactive({
      infile <- input$file
      if (is.null(infile)) {
        return(readLines("www/ObamaSpeech.txt"))
      }
      readLines(infile$datapath)
    })
    
    sentiment_scores <- reactive({
      file_data() %>%
        enframe(name = NULL, value = "text") %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("afinn"), by = "word") %>%
        group_by(value) %>%
        summarise(sentiment_score = sum(value))
    })
    
    # Generate the sentiment plot
    output$sentiment_plot <- renderPlotly({
      ggplot(sentiment_scores(), aes(x = factor(ifelse(sentiment_score > 0, "Positive", "Negative")), y = sentiment_score)) +
        geom_bar(stat = "identity", fill = ifelse(sentiment_scores()$sentiment_score > 0, "#DAF7A6", "#DE394F")) +
        labs(x = "Sentiment", y = "Total Score", title = "Sentiment Analysis Results") +
        theme_minimal()
    })
    
    # Generate the emotion plot
    output$emotion_plot <- renderPlotly({
      file_data() %>%
        as_tibble() %>%
        unnest_tokens(word, value) %>%
        inner_join(get_sentiments("nrc"), by = "word") %>%
        group_by(sentiment) %>%
        summarise(count = n()) %>%
        ggplot(aes(x = reorder(sentiment, count), y = count)) +
        geom_bar(stat = "identity", fill = "dodgerblue") +
        coord_flip() +
        labs(x = "Emotion", y = "Count", title = "Emotion Classification Results") +
        theme_minimal()
    })
  
  # Generate the emotion plot
  output$emotion_plot <- renderPlotly({
    temp <- file_data() %>%
      as_tibble() %>%
      unnest_tokens(word, value) %>%
      inner_join(get_sentiments("nrc"), by = "word") %>%
      group_by(sentiment) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = reorder(sentiment, count), y = count)) +
      geom_bar(stat = "identity", fill = "dodgerblue") +
      coord_flip() +
      labs(x = "Emotion", y = "Count", title = "Emotion Classification Results") +
      theme_minimal() 
    ggplotly(temp, tooltip = c("y")) %>%
      layout(hovermode = "closest")
  })
  
  #Word Cloud -----   
  comments <- reactive({
    if (input$submit > 0) {
      url <- input$url
      if (!grepl("^https?://(.*)", url)) 
        url <- paste0("https://www.", gsub("^.*(reddit\\..*$)", 
                                           "\\1", url))
      if (!grepl("\\?ref=search_posts$", url)) 
        url <- paste0(gsub("/$", "", url), "/?ref=search_posts")
      X <- paste0(gsub("\\?ref=search_posts$", "", url), 
                  ".json?limit=500")
      raw_data <- tryCatch(RJSONIO::fromJSON(readLines(X, warn = FALSE), nullValue = "none"), 
                           error = function(e) NULL)
      if (is.null(raw_data)) {
        Sys.sleep(min(1, wait_time))
        raw_data <- tryCatch(RJSONIO::fromJSON(readLines(X, 
                                                         warn = FALSE), nullValue = "none"), error = function(e) NULL)
      }
      if (is.null(raw_data) == FALSE) {
        main.node <- raw_data[[2]]$data$children
        if (length(main.node) > 0) {
          comments <- unlist(lapply(main.node, function(x) {
            GetAttribute(x, "body")
          }))
        }
      }
      comments
    }
  })
  
  output$wordcloud <- renderPlot({
    if (!is.null(comments())) {
      wordcloud(comments(), scale=c(2, 0.25), min.freq = 4, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
    }
  })
# end-----
    
})