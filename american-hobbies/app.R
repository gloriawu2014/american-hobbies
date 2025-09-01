# ------------------------------------------------------------------------------
# American Hobbies
# Author: Gloria Wu, Justyce Williams, Ben Snyderman
# Date: 4.4.25
# ------------------------------------------------------------------------------

# =============================================================================
# American Hobbies App - Team Member Contributions
# Tab 1: Home Page - Justyce
# Tab 2: Scatterplot - Justyce
# Tab 3: Table - Ben
# Tab 4: Map - Gloria
# Tab 5: About & References - Gloria
# =============================================================================

library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(stringr)
library(bslib)        # for functions controlling the user interface layout 
library(DT)           # for interactive tables
library(ggrepel)      # to add labels to scatterplot that don't overlap the points
library(plotly)       # for interactivity with ggplot
library(RColorBrewer)
library(paletteer)


# load in dataset - set file as working directory
load("data/final_data.Rdata")

# contains all the states from the dataset (for table)
state_choices <- unique(table_data$state_name)

fullfile <- fullfile |>
  rename(Hobbies = thirdtier)

## server
server <- function(input, output){
  
# scatterplot
  output$scatter <- renderPlot({
    fullfile |>
      filter(Hobbies %in% input$hobbs) |>
      slice_sample(n = input$max)|>
      ggplot(aes(x = duration/60, y = earnings/1000, color = age_group)) +
      geom_point()+
      facet_wrap(~as.factor(age_group))+
      scale_color_manual(values = c("#90CAF9FF", "#1565C0FF", "#0D47A1FF", "black", "#42A5F5FF" ),
                        labels = c("Adolescence","Young Adult", "Adult", "Older Adult", "Senior"),
                        name = "Age Group") +
      labs( x = "Hours per Week "
          , y = "Salary of Individual CR*"
          , title = paste("Distribution of Time Spent: "
                             , input$hobbs)
          , subtitle = "American Census Data 2023 
Earnings Reported in Thousands"
          ,caption = "Census Responder*")+
      #Making colors and text larger 
      theme(plot.title = element_text(size = 20,
                         face = "italic"), 
            axis.title.x = element_text(size = 16),  
            axis.title.y = element_text (size = 16)
      )

  })
 
# scatter separation by gender to tell a different story 
 output$scatter2 <- renderPlot({
    fullfile |>
      filter(Hobbies %in% input$hobbs) |>
      slice_sample(n = input$max) |>
      ggplot(aes(x = duration/60, y = earnings/1000)) +
      geom_point(aes(color = as.factor(Gender))) +
      labs(   x = "Hours per Week "
            , y = "Salary of Individual CR*"
            , title = paste("Distribution of Time Spent: "
                            , input$hobbs),
            subtitle = "American Census Data 2023"
            ) + scale_color_manual(values = c("blue", "deepskyblue"),
                       name = "Gender", 
                       labels = c("Male","Female")) +  
 theme(plot.title= element_text(size = 20,
                    face = "italic"), 
         axis.title.x = element_text(size = 16),  
         axis.title.y = element_text(size = 16)
     )
 })
  
  # table
  # where the actual table is made to render
  # contains an if statement which makes it so that if “All” is selected, then all the states are shown
  # also takes in all user inputs for the rows and then another for the columns
  data_for_table <- reactive({
    if("All" %in% input$state){
      table_data |>
        rename(State = state_name)
    } else{
    table_data |>
      rename(State = state_name) |>
      filter(State %in% input$state)}
  })

  output$table <- DT::renderDataTable({
    data_for_table()[input$columns]
  })

  # ggplot map
  mymap <- ggplot() + 
    geom_sf(data = states, aes(geometry = geom), fill = NA) +
    geom_sf(data = avg_time, aes(geometry = geom, fill = time, text = text)) +
    scale_fill_continuous(low = "lightblue", high = "purple") +
    theme_void() +
    labs(title = "Average Time Spent on Hobbies by Location per Day",
         fill = "Average Time")
  
  # make map interactive
  output$map <- renderPlotly({
    ggplotly(mymap, tooltip = "text")
  })
  
  # interactive data table for map
  output$table_map <- DT::renderDataTable({
    table_map_data
  })
  
}

## user interface
ui <- page_navbar(
  
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  title = "American Hobbies",
  
  nav_panel(
        title = "Home Page", 
        h3("Interested In Exploring American Hobbies?",  style = "color: navy;"),
        p(HTML("Hobbies give us time to ourselves and are a way to connect with others. 
          In a place as diverse as the U.S., people are into all kinds of 
          leisure activities such as watching Netflix, hiking, gaming, crafting, and many more. 
          It’s cool how something as simple as a shared interest 
          can bring people together,
          or how doing something solo can help us recharge. <br><br>
          
          Hobbies are always an interesting topic of discussion, and people
          love talking about them -- even reporting them. 
          Many reported their hobbies for the 2023 Census. Look at the results. 
          ")),
        
        h3("Components of Each Display", style = "color: navy;"),
        p(HTML("Scatterplot:<br>

          Curious about how responders earnings and time spent can relate to one another?<br>
          Curious about how age or gender can look across those outcomes?<br>  

          The sidebar give you access to different hobbies that interest you, 
          choose one and see how that hobby’s outcome looks across relationships
          of earnings, age, time and gender. Slide the bar to remove or add 
          data points to digest the data that works for you.<br><br>

          Table: <br>

          Wonder how location impacts what people choose to spend their time on?<br>

          The first select box allows you to choose state(s), select your states, 
          and compare them against neighboring states if you’d like. 
          Hobbies can be grouped into a few different categories. 
          The second select box different categories, and see how the different 
          times compare amongst a state itself, and others. <br><br>

          Map: <br>

          Wonder if there is a difference in overall time spent on hobbies by location?<br>

          The color gradient delineates how average time spent 
          on hobbies changes across states.
          Drag your mouse across the map to learn more about the average number of
          minutes spent on hobbies, 
          the average ages, and average weekly earnings within each state. 
          Below the map is a comprehensive table reporting the findings from the map.
               Use the search bar for specific states. ")),
        h3("Remarks", style = "color: navy;"),
        p(HTML("Through exploring this data, our team learned about the 
        different ways people in America recharge and connect with one another.
        Below you will find a list of those findings: <br><br>
             
        The Gym: <br>
        Gym culture remained strong in 2023, with it being one of the most 
        frequently reported hobbies in the 2023 Census. If these trends 
        continue, gym influencers are likely to thrive even further. 
        Healthy gym habits can contribute to increased confidence, 
        peace of mind, improved energy levels, and 
        stronger social connections with fellow gym-goers. 
        Explore the gym along with other hobbies on the scatterplot tab 
        located at the top of the page. <br><br>
        
        Illinois vs Colorado <br>

        Although Illinois has multiple baseball teams and has all 4 
        major sports teams, Colorado had a significantly higher average time 
        spent attending sports. This is probably why Denver has the reputation 
        for being a very passionate sports city and is one of the smallest 
        cities to contain all 4 major sports teams."))
  ),
  
  # nav_panel for the table
  # makes the input variable of state, then adds the state options and an "All" option.
  # Then adds options for the columns with the activities then has headers explaining what the table is
  nav_panel(
    title = "Scatterplot",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("hobbs", "Choose a Hobby:", choices = unique(fullfile$Hobbies)),
             sliderInput("max", "Max Number of Responders:", min = 1,
                         max = 1000, value = 500)), #1000 because the most 
                                                    #entries for a singular 
                                                    #hobby is 951
        card(
          plotOutput(outputId = "scatter")
        ), 
      card(
        plotOutput(outputId = "scatter2")
      )
    )
  ),

  nav_panel(
    title = "Table",
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput(inputId = "state"
                       , label = "Choose one or more states:"
                       , choices = c("All", state_choices)
                       , selected = c("Illinois", "Colorado", "Virginia", "California", "New York")
                       , multiple = TRUE),
        selectizeInput(inputId = "columns",
                       label = "Choose columns to display:",
                       choices = c("State", "Socializing/Communicating", 
                                   "Attending/Hosting Social Events", 
                                   "Relaxing/Leisure", "Arts/Entertainment", 
                                   "Participating in Sports/Exercise/Recreation", 
                                   "Attending Sporting/Recreation Events", 
                                   "Administrative & Support Activities", 
                                   "Social Service & Care Activities (Except Medical)", 
                                   "Indoor & Outdoor Maintenance, Building, & Clean-up Activities"),
                       selected = c("State", "Socializing/Communicating", "Arts/Entertainment"),
                       multiple = TRUE)
      ),
      card(
        h3("How Different States Spend Their Time"),
        p("Average time spent on each activity per respondent in a given week."),
        p("*All times are in minutes*"),
        DT::dataTableOutput(outputId = "table")
      )
    )
  ),
  
  nav_panel(
    title = "Map",
    card(
      plotlyOutput(outputId = "map"),
      p("There is no data for Wyoming, Hawaii, and Alaska."),
    ),
    card(
      DT::dataTableOutput(outputId = "table_map")
    )
  ),
  
  nav_panel(
    title = "About",
    h3("Purpose", style = "color: navy;"),
      p("The purpose of this project is to see how people spend their time on 
      various leisure activities across different locations, age groups, 
      and tax brackets.
      Leisure activities are defined within the categories of socializing, 
        relaxing, leisure, sports, exercise, recreation, and volunteering."),
    h3("Datasets", style = "color: navy;"),
      p("Data was collected from the 2023 American Time Use Survey, 
      a federally administered, continuous survey on time use in the U.S.
      Respondents were randomly selected from households that have completed 
      interviews for the Current Population Survey.
      Information on labor force status, earnings, age, sex, location, 
      and various activities were collected.
      Additionally, the CBSA to FIPS dataset was used to map locations."),
    h3("References", style = "color: blue;"),
    HTML(paste(tags$a(href = "https://www.bls.gov/tus/data/datafiles-2023.html", "American Time Use Survey"))),
    HTML(paste(tags$a(href = "https://www.nber.org/research/data/census-core-based-statistical-area-cbsa-federal-information-processing-series-fips-county-crosswalk",
                      "CBSA to FIPS"))),
    h3("Team Members", style = "color: navy;"),
         p(HTML("Gloria Wu <br>
           Justyce Williams <br>
           Ben Snyderman <br>")
  ))
)

shinyApp(ui = ui, server = server)

