# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# Developed with R version 3.3.2 (64-bit)
# Packages ----
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(shiny)
library(leaflet)
library(readxl)
library(htmltools)
library(rsconnect)
library(leaflet.extras)
library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)
library(textdata)
library(tidytext)
library(tidyverse)
library(RedditExtractoR)
library(wordcloud)
library(tm)
library(slam)
library(RJSONIO)


source("carouselPanel.R")
source("datfilter.R")

# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content)
    )
}

shinyUI(navbarPage(title = div(img(src="SFU_Logo.png", height = "40px",),img(src="citystudio.png", height = "40px",),),id = "navBar",
                   theme = "paper.css",
                   collapsible = TRUE,
                   inverse = TRUE,
                   windowTitle = "SDA490",
                   position = "fixed-top",
                   footer = includeHTML("./www/test_html.html"),
                   header = tags$style(
                       ".navbar-right {
                       float: right !important;
                       }",
                       "body {padding-top: 68px;}"),
# Home ----------

                   tabPanel("Home", value = "home", icon = icon("house"),
                            
                            shinyjs::useShinyjs(),
                            
                            tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                            for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                            
                            fluidRow(
                                HTML("
                                     
                                     <section class='banner'>
                                     <h2 class='parallax' style='color:White;'>Welcome to our Website!</h2>
                                     <p class='parallax_description'>Project Data Showcase</p>
                                     </section>
                                     ")
                                ),
                            
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>Capstone Project 2023:</h1> </center><br>"),
                                       shiny::HTML("<h5>Understanding Vancouver's Traffic, 
                                       an exploratory data analysis for Simon Fraser University SDA 490 Spring 2023. </h5>")
                                       ),
                                column(3)
                                       ),
                            
                            fluidRow(
                                
                                style = "height:150px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                        
                                fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>What is this: </h1> </center><br>"),
                                       shiny::HTML("<h5>This is an interactive tool designed to help you explore certain
                                       aspects of Vancouver BC traffic as well as byproducts of car ownership, alongside 
                                       textual sentiment analysis based upon real opinions.</h5>")
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:150px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # HOW
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>Our Project Direction</h1> </center><br>"),
                                       shiny::HTML("<h5>Our group aims to empower Lower Mainland residents with information 
                                                   to make better decisions about the city's infrastructure, with a focus on 
                                                   transportation education. Our goal is to influence policy makers in British 
                                                   Columbia to reduce traffic and optimize existing traffic by improving infrastructure.
                                                   We will analyze data on current transportation patterns and infrastructure, 
                                                   as well as gather feedback from residents and stakeholders to better understand their
                                                   needs and concerns. </h5>"),
                                       
                                       ),
                                column(3)
                                       ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # WHERE
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>What you should expect to see</h1> </center><br>"),
                                       shiny::HTML("<h5><ul>
                                                <li>Our inspiration and research question</li>
                                                <li>Our hypotheses</li>
                                                <li>A collection of interactive heatmaps</li>
                                                <li>A collection of Various interactive plots</li>
                                                <li>Interactive textual sentiment analysis and emotion classifier</li>
                                                <li>Findings and explanations</li>
                                                <li> Suggestions and cohesive conclusion</li>
                                                   </ul></h5>"),
                                       ),
                                column(3)
                                       ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # 
                            fluidRow(
                                column(3),
                                column(7,
                                       shiny::HTML("<br><br><center> <h2>How will traffic calming driving penalties and 
                                       its subsequent non-driving rewards affect Vancouver traffic? </h2> </center><br>"),
                                       shiny::HTML("<h5>This question is important to us as a group
                                       because as economics students we understand that most everything comes with costs 
                                       associated with it.</h5>"),
                                       
                                       shiny::HTML("<h5>Do you want to get downtown in the comfort of your own automobile? 
                                       Well be prepared for the cost of traffic. Alternatively, if you want to get downtown 
                                       quickly? Take the bus, but be prepared for abrupt stops, little personal space, and other 
                                       costs of taking alternative forms of transportation.</h5>"),
                                       
                                       shiny::HTML("<h5>This is just a simple example of our economic approach to thinking about 
                                       how to both understand and alleviate traffic, while encouraging alternative forms of 
                                                   transportation</h5>"),
                                       
                                       fluidRow(
                                           
                                           style = "height:250px;"),
                                       
                                       shiny::HTML("<h4>Some of our hypotheses include:</h4>"),
                                       shiny::HTML("<h5><ol>
                                                <li>Increasing the number of traffic controls will have a reduction on 
                                                private motor vehicle usage.</li>
                                                <li>Reduction of motor vehicle usage changes with the severity of traffic control</li>
                                                <li>The size of roads/sidewalks will have an effect on the amount of foot and bicycle 
                                                traffic with respect to car traffic and vice versa.</li>
                                                <li>The amount of traffic accidents is determined by the surrounding traffic 
                                                infrastructure/lack of traffic controls.</li>
                                               </ol></h5>"),
                                       shiny::HTML("<h5> As you will see as you explore this application, testing these hypotheses 
                                       was of large importance for us.</h5>"),
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            #new
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>How this R Shiny web app can help you.</h1> </center><br>"),
                                       shiny::HTML("<h5>This app was developed with the education of the user at the forefront of our
                                       minds. We think that the first step to solving traffic issues in Vancouver is through education 
                                       in an interactive way to help users rethink their need for a vehicle.</h5>"),
                                       shiny::HTML("<h5>Imperfect information causes inefficiency, and as economics students, 
                                       where we see the ability for optimization, we will take it. Education is a barrier to entry
                                       for many, because “you don’t know what you don't know”. This fallacy in this case, leads to 
                                       overpopulated streets, lower translink ridership, as well as increased crime, which will be 
                                       highlighted in this exploratory data analysis.</h5>"),
                                       shiny::HTML("<h5>We hope that you discover something new about the way Vancouver 
                                       traffic and its byproducts affect the city.</h5>"),
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            #end new
                            
                            #new
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>Where did it come from?</h1> </center><br>"),
                                       shiny::HTML("<h5>Our team analyzed many types and styles of data from over a decade and transformed
                                       this information into what you will see and experience during the presentation of this R Shiny web 
                                        app.</h5>"),
                                       shiny::HTML("<br><br><center> <h1>Data used during this project includes:</h1> </center><br>"),
                                       shiny::HTML("<h5><ul>
                                        <li>Census Data.</li>
                                        <li>City of Vancouver intersection data.</li>
                                        <li>ICBC collision and traffic violation data.</li>
                                        <li>Traffic signal data.</li>
                                        <li>City of Vancouver crime data.</li>
                                        <li>Government of Canada job data.</li>
                                        <li>Scraped text data from the internet (twitter, reddit, etc).</li>
                                        </ul></h5>"),
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            #end new
                            
                            #new
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>How does it fit into the big picture??</h1> </center><br>"),
                                       shiny::HTML("<h5>Building a clear path is just one part of effective infrastructure/city 
                                       planning and development. You should also establish a city plan to outline how you will 
                                       achieve these goals. This illustration hopes to provide you with a better understanding 
                                       and a realistic path to help change the reality of lower mainland traffic</h5>"),
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            #end new
                            
                            #start of the interactive home page ----
                            # HOW TO START
                            # fluidRow(
                            #     column(3),
                            #     column(6,
                            #            shiny::HTML("<br><br><center> <h1>How to get started</h1> </center><br>"),
                            #            shiny::HTML("<h5>To launch the tab2, click->click->click.</h5>")
                            #            ),
                            #     column(3)
                            #            ),
                            # 
                            # # BUTTONS TO Click
                            # fluidRow(
                            #     column(3),
                            #     column(6,
                            #            
                            #            tags$div(class = "wrap",
                            #                     div(class = "center", 
                            #                         style="display: inline-block;vertical-align:top; width: 225px;",
                            #                         tags$a("Website link",
                            #                                onclick = "window.open('https://www.tidytextmining.com/topicmodeling.html#document-topic-probabilities', '_blank')",
                            #                                class="btn btn-primary btn-lg")
                            #                     ),
                            #                     div(class = "center",
                            #                         style="display: inline-block; vertical-align:top; horizontal-align:middle; width: 75px;",
                            #                         tags$br(), tags$h4("OR") ),
                            #                     div(class = "center",
                            #                         style="display: inline-block;vertical-align:top; width: 225px;",
                            #                         tags$a("clcik to go tab 2", 
                            #                                onclick="fakeClick('tab2')", 
                            #                                class="btn btn-primary btn-lg")
                            #                     )
                            #            )
                            #     ),
                            #     column(3)
                            # ),
                            # 
                            # fluidRow(
                            #     
                            #     style = "height:50px;"),
                            # 
                            # # PAGE BREAK
                            # tags$hr(),
                            # 
                            # # INSTRUCTIONAL SECTION
                            # fluidRow(
                            #     shiny::HTML("<br><br><center> <h1>123</h1> </center>
                            #                 <br>")
                            #     ),
                            # 
                            # fluidRow(
                            #     column(3),
                            #     
                            #     column(2,
                            #            div(class="panel panel-default", 
                            #                div(class="panel-body",  width = "600px",
                            #                    align = "center",
                            #                    div(
                            #                        tags$img(src = "one.svg", 
                            #                                 width = "50px", height = "50px")
                            #                    ),
                            #                    div(
                            #                        h5(
                            #                            "Step1"
                            #                        )
                            #                    )
                            #                )
                            #            )
                            #     ),
                            #     column(2,
                            #            div(class="panel panel-default",
                            #                div(class="panel-body",  width = "600px", 
                            #                    align = "center",
                            #                    div(
                            #                        tags$img(src = "two.svg", 
                            #                                 width = "50px", height = "50px")
                            #                    ),
                            #                    div(
                            #                        h5(
                            #                            "Step 2"
                            #                        )
                            #                    )
                            #                )
                            #            )
                            #     ),
                            #     column(2,
                            #            div(class="panel panel-default",
                            #                div(class="panel-body",  width = "600px", 
                            #                    align = "center",
                            #                    div(
                            #                        tags$img(src = "three.svg", 
                            #                                 width = "50px", height = "50px")),
                            #                    div(
                            #                        h5(
                            #                            "Step 3"
                            #                        )
                            #                    )
                            #                )
                            #            )
                            #     ),
                            #     column(3)
                            #     
                            # ),
                            # 
                            # # Embedded Video from Vimeo on how to use this tool
                            # # fluidRow(
                            # #     column(3),
                            # #     column(6,
                            # #            tags$embed(src = "https://player.vimeo.com/video/8419440",
                            # #                       width = "640", height = "360") 
                            # #     ),
                            # #     column(3)
                            # # ),
                            # 
                            # fluidRow(
                            #     
                            #     style = "height:50px;"),
                            # 
                            # # PAGE BREAK
                            # tags$hr(),
                            # 
                            # # AFTERWARD
                            # fluidRow(
                            #     column(3),
                            #     column(6,
                            #            shiny::HTML("<br><br><center> <h1>How does it fit in the big picture?</h1> </center><br>"),
                            #            shiny::HTML("<h5>Building a clear path is just one part of effective infrastructure/city 
                            #                        planning and development. You should also establish a city plan 
                            #                        to outline <i>how</i> you will achieve your goals. Our
                            #                        illsration hopes provides information to help you establish 
                            #                        a plan for making your path a reality.</h5>")
                            #            ),
                            #     column(3)
                            #            ),
                            # 
                            # fluidRow(
                            #     
                            #     style = "height:50px;"),
                            # 
                            # # PAGE BREAK
                            # tags$hr(),
                            # 
                            # fluidRow(shiny::HTML("<br><br><center> <h1>Ready to Get Started?</h1> </center>
                            #                      <br>")
                            # ),
                            # fluidRow(
                            #     column(3),
                            #     column(6,
                            #            tags$div(align = "center", 
                            #                     tags$a("Start", 
                            #                            onclick="fakeClick('tab2')", 
                            #                            class="btn btn-primary btn-lg")
                            #            )
                            #     ),
                            #     column(3)
                            # ),
                            # fluidRow(style = "height:25px;"
                            # )
                            #THIS IS interactive home page
                            
                            ), # Closes the first tabPanel called "Home"
                   

# Tab2 ----
            navbarMenu("Crash Data",  icon = icon("car-burst"),
# tab2(all years)----                       
                    tabPanel("All Years Included", 
                             div(class = "row", 
                                 column(9,titlePanel("Intersection Crash Count")),
                                 column(9,align = "center",leafletOutput("mySecondMap")),
                             div(shiny::HTML("<h3>Illstration 1: Crash Data</h3>"),
                                 shiny::HTML("This map provides an overview of traffic crashes in Vancouver between 2017 and 2021. It includes accidents with both casualties and without casualties.
                                             The map highlights areas of crash frequency. 
                                             We can see the crash frequency is the <b>highest</b> particularly at <b><i>intersections and exits/entrances</i></b> of highways. 
                                             By visualizing the crash data geographically, we can identify patterns and trends in areas that require attention and interventions. 
                                             Overall, this map is an important tool for identifying areas where traffic safety improvements are needed and for supporting evidence-based 
                                             decision making to reduce the number of traffic crashes and injuries in Vancouver.
                                             <br>(fyi: Crash Counts are sorted by percentile. For example, Crash amount greater than 600 is <b><i>~ 99.94</i></b> percentile.)"))
                             #res<-quantile(groupedData$totalCrashes,probs=0.9994)
                                 
                                 
                             ),
                             div(class = "row",
                                 column(9,titlePanel("Intersection Crash Count (Clustered)")), 
                                 column(9,align = "center",leafletOutput("myMap")),
                                 div(shiny::HTML("<h3> Illstration 2: Street-View Crash Data </h3>"),
                                 shiny::HTML("This second map, which also uses traffic crashes data from Vancouver between 2017 and 2021, 
                                             provides a more <b>granular view</b> of the information compared to the previous map. It uses hierarchical clustering algorithm to groups similar objects into clusters based on their distance.
                                             It allows users to explore the data at the street level, 
                                             with the ability to click on specific intersections to see their corresponding crash information. 
                                             This makes it easier to gain insights and identify patterns that may not have been visible in the 
                                             previous map."))
                                 
                             ),
                             
                             
                    ),
# tab2(others)----
                     tabPanel("Year Selection", 
                              div(class = "row",
                                  column(9,titlePanel("Heatmap By Years")),
                                  column(9, selectInput("year", "Select a year:", unique(yearsData$Year))),
                                  column(9,align = "center",leafletOutput("thirdMap")),
                                  shiny::HTML("<h3>Illstration 3: Heatmap of Crashes Sorted by Years.</h3>"),
                                  shiny::HTML("The interactive map provides an overview of traffic crashes in Vancouver for each year. 
                                              Similar to Illustration 1, this map displays areas of crash frequency from high (red) to low 
                                              (yellow), but with the added benefit of allowing users to select different years and see how 
                                              the patterns of crashes change over time. This makes the map especially useful for identifying  
                                              trends and <b><i>patterns based on time</b></i>."))
                             
                             
                            )
                    ),  # Closes the second tabPanel called "Maps2"

# Tab3 ----
            navbarMenu("Signals", icon = icon("traffic-light"),
                       tabPanel("Traffic Signals",
                                titlePanel("Traffic Signals"),
                                sidebarLayout(
                                    sidebarPanel(
                                        shiny::HTML("<h5>Overlay with Crash heatmap</h5>"),
                                        
                                        # checkboxInput(
                                        #     inputId = "show_markers",
                                        #     label = "Show greater than 250 crashes",
                                        #     value = TRUE
                                        # ),
                                        checkboxGroupInput(
                                            inputId = "signal_types",
                                            label = "Select signal types:",
                                            choices = c(
                                                "Pedestrian Actuated Signal",
                                                "Semi Actuated",
                                                "Fixed Time",
                                                "RRFB",
                                                "Fully Actuated",
                                                "Special Crosswalk",
                                                "Bus Actuated Signal",
                                                "FH",
                                                "CS"),
                                            selected = c(
                                                "Pedestrian Actuated Signal",
                                                "Semi Actuated",
                                                "Fixed Time"))),#3 )'s
                                  
                                        
                                    mainPanel(
                                        # Leaflet map
                                        leafletOutput(outputId = "signal_map"),
                                        fluidRow(
                                            
                                            style = "height:50px;"),
                                        
                                        ) #close main panel
                                    
                                    
                                    ) 
                                )
                       ),



# Tab4 ----         
            navbarMenu("Other Exogenous Factors",icon = icon("people-robbery"),
                       tabPanel("Crime In Vancouver", 
                                div(class = "row", 
                                    column(9,titlePanel("Share of Crime Across Vancouver Neighborhoods")),
                                    column(9,align = "center",plotlyOutput("plot1")),
                                    div(shiny::HTML("<h3>Illustration 4: </h3>"),
                                        shiny::HTML("This bar graph displays the number of vehicle thefts in a specific neighborhood. According to the number of thefts, the neighborhoods are arranged in decreasing order. The graph can be helpful to law enforcement in identifying neighborhoods where theft from vehicles occurs more frequently. Additionally, we may draw a connection between the increase in vehicles and the rise in crime. 
            "))
                                ),
                                div(class = "row", 
                                    column(9,titlePanel("Share of Crime Across Vancouver Neighborhoods Per Capita")),
                                    column(9,align = "center",plotlyOutput("barplot")),
                                    div(shiny::HTML("<h3>Illustration 5: </h3>"),
                                        shiny::HTML("This bar graph displays the number of vehicle thefts in a specific neighborhood per capita. According to the number of thefts, the neighborhoods are arranged in decreasing order. The graph can be helpful to law enforcement in identifying neighborhoods where theft from vehicles occurs more frequently. Additionally, we may draw a connection between the increase in vehicles and the rise in crime. 
            "))
                                )
                                
                                
                                
                       ),
                       tabPanel("Peoples Drive Distance",        
                                div(class = "row", 
                                    column(9,titlePanel("Vehicle Kilometer Travelled Per Capita")),
                                    column(9,align = "center",plotlyOutput("vkt_bar")),
                                    div(shiny::HTML("<h3>Illustration 6: </h3>"),
                                        shiny::HTML("This bar graph shows the Vehicle Kilometer Travelled (VKT) per capita by city in a bar chart format. It helps us to compare the VKT per capita values for different cities in Metro Vancouver. The higher the bar, the higher the VKT per capita. The custom color palette based on the city names makes it easier to differentiate between the bars representing different cities. This information can be useful for understanding transportation pattern."))
                                )),
                       #
                       tabPanel("Job Market Share", 
                                div(class = "row", 
                                    column(9,titlePanel("Employment Factors")),
                                    column(9,align = "center",plotlyOutput("plot2")),
                                    div(shiny::HTML("<h3>Illustration 7: </h3>"),
                                        shiny::HTML("The bar graph shows the percentage of jobs in different cities of Metro Vancouver in British Columbia, Canada. Each bar represents a city, and its length represents the percentage of jobs in that city.  It provides valuable insights into which cities have more employment opportunities. Overall, this graph provides a quick and easy-to-understand overview of job distribution in Metro Vancouver. 
            "))
                                )),
                       #fatalities 
                       tabPanel("Fatalities Factors",
                                titlePanel("Share of Vehicular Fatalities"),
                                sidebarLayout(
                                    sidebarPanel(
                                        selectInput("factor", "Factor:", choices = unique(dataV$Factor))
                                    ),
                                    mainPanel(
                                        plotOutput("plot3")
                                    )
                                )
                       )
            ),
            
            
                               
            
            # Tab5 ----         
                  navbarMenu("Text Scraping",icon = icon("file-lines"),
                                   tabPanel("Text file Scraping",
                                       fileInput("file", "Choose a text file to analyze"),
                                       plotlyOutput("sentiment_plot"),
                                       plotlyOutput("emotion_plot")
                                   ),
                             tabPanel("Scraping Reddit",
                                      titlePanel("Reddit Word Cloud"),
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput("url", "Please enter a valid Reddit.com URL"),
                                          actionButton("submit", "Submit")
                                        ),
                                        mainPanel(
                                          plotlyOutput("wordcloud"))
                                      )
                             )
                        ),




#About tab ----
                   tabPanel("About", value = "about",
                            
                            fluidRow(
                                column(3),
                                column(6,
                                shiny::HTML("<br><b><center><h1>About our project</h1> </center> </b></br>"),
                                shiny::HTML("That's the end of our intectactive website. Thank you for checking this out!
                                            We hope that you'll enjoy exploring this interactive tool and that you'll 
                                            discover something new about the way Vancouver's traffic and its 
                                            byproducts affect the city."),
                                
                            )),
                            
                             column(3),
                            
                            fluidRow(
                                
                                style = "height:100px;"),
                            
                           
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h5>Final Regards</h5> </center><br>"),
                                       shiny::HTML("<h6>This project is organized by Steven Weldon and Edana Beauvais from SFU, with collaboration from City of Vancouver and City Studio. Thank you for giving us this opportunity! And here is a little information 
                                                   about the project team!</h6>")
                                       ),
                                column(3)
                                       ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            fluidRow(
                                column(3),
                                
                                # Marc
                                column(2,
                                       div(class="panel panel-default", 
                                           div(class="panel-body",  width = "600px",
                                               align = "center",
                                               div(
                                                   tags$img(src = "man.svg", 
                                                            width = "50px", height = "50px")
                                               ),
                                               div(
                                                   tags$h5("Dustin"),
                                                   tags$h6( tags$i("Visionary, Project Leader, Web Designer, Main Coder, Editor"))
                                               ),
                                               div(
                                                   "Grateful for collaborating on a project with potential to positively impact our community."
                                               )
                                           )
                                       )
                                ),
                                # George
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                   tags$img(src = "man_beard_1.svg", 
                                                            width = "50px", height = "50px")
                                               ),
                                               div(
                                                   tags$h5("Matt"),
                                                   tags$h6( tags$i("Bibliographer/Editor, Texual Analyst/Web Scraper, Support Coder"))
                                               ),
                                               div(
                                                   "Happy to have gained R Shiny experience and applied it in a meaningful way for the City of Vancouver"
                                               )
                                           )
                                       )
                                ),
                                # Angela
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                   tags$img(src = "man_beard_2.svg", 
                                                            width = "50px", height = "50px")),
                                               div(
                                                   tags$h5("Amir"),
                                                   tags$h6( tags$i("Data Collecter/Wrangler, Graph Builder, Visualization Specialist"))
                                               ),
                                               div(
                                                   "Happy to have worked with real life data and applied it with a larger goal in mind."
                                               )
                                           )
                                       )
                                ),
                                column(3)
                                
                            ),
                            fluidRow(style = "height:150px;")
                        )  # Closes About tab
                   
                            )
)
        
                   