
library(shiny)
# library(shinySignals)   # devtools::install_github("hadley/shinySignals")
library(dplyr)
library(shinydashboard)
library(bubbles)        # devtools::install_github("jcheng5/bubbles")#
library(tidyverse)
library(countrycode)
library(scales)
library(viridis)
library(plotly)
library(glue)
library(RColorBrewer)

## Start Year and End Year
first_year = 2010
last_year = 2020


## Read in data 
{
    renewable_investment <- read.csv("Renewable_investment.csv")
    world_energy <- read.csv("World_Energy.csv", header= TRUE)
    names(world_energy) <- c("Country","Indicator",seq(2000,2015))
    GDP <- read.csv("GDP.csv")
    names(GDP) <- c("Country","Country_Code","Indicator_Name","Indicator_Code",seq(1960,2020,1))
    
}


## Transform the data (Renewable Investment)
{
    
    renewable_investment_long <- renewable_investment %>% 
        pivot_longer(cols = paste0("X",seq(first_year,last_year,1)),names_to = "Year",values_to = "Renewable Investment") %>% 
        mutate(Year= as.integer(gsub("X","",Year)),
               `Renewable Investment` =`Renewable Investment`/1e9)
    
    names(renewable_investment_long) <- c("Country","Year","Renewable Investment")
    
    renewable_investment_long$Continent <- countrycode(sourcevar = as.data.frame(renewable_investment_long)[, "Country"],
                                                       origin = "country.name",
                                                       destination = "continent")
    
    renewable_investment_long$Continent[renewable_investment_long$Country=="Ivore Coast"] <- "Africa" # manually assign it to Africa
    
}


## Transform the data (World Energy)
{
    
    world_energy_1 <- world_energy %>% 
        pivot_longer(cols = as.character(seq(2000,2015,1)), names_to = "Year",values_to= "value")
    
    world_energy_1$Continent <- countrycode(sourcevar = as.data.frame(world_energy_1)[, "Country"],
                                            origin = "country.name",
                                            destination = "continent")
    
    world_energy_1$Continent[world_energy_1$Country=="Ivore Coast"] <- "Africa" # manually assign it to Africa
    
    world_energy_2 <- world_energy_1 %>% 
        pivot_wider(id_cols = c("Year","Continent","Country"), names_from = "Indicator",values_from = "value") %>% 
        mutate(Year = as.integer(Year))
    
    
}


## Transform the data (GDP)
{
  GDP_1 <- GDP %>% 
    select(-Indicator_Name) %>% 
    select(-Indicator_Code) %>% 
    select(-Country_Code) %>%
    pivot_longer(cols = as.character(seq(1960,2020,1))) %>% 
    group_by(Country) %>% 
    fill(value, .direction ="down") %>% 
    rename(Year=name,
           GDP =value) %>% 
    mutate(Year= as.integer(Year))
  
  
}

# Merge world_energy data with GDP
{
  world_energy_gdp <- world_energy_2 %>% 
    left_join(GDP_1, on = c("Country","Year"))
}

# Compute global statistics
{
    total_renewable_investment_last <- sum(renewable_investment_long$`Renewable Investment`[renewable_investment_long$Year==last_year])
    total_renewable_investment_prior_to_last <- sum(renewable_investment_long$`Renewable Investment`[renewable_investment_long$Year==last_year-1])
    yoy <- (total_renewable_investment_last- total_renewable_investment_prior_to_last)/total_renewable_investment_prior_to_last * 100
}




# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Renewable Trends"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "mainpage"),
            menuItem("Trend", tabName = "trend"),
            menuItem("Opportunity", tabName = "opportunity"),
            menuItem("Summary", tabName = "summary")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("mainpage",
                    fluidRow(
                        valueBoxOutput("total_investment_dollar"),
                        valueBoxOutput("year_of_year"),
                    ),
                    fluidRow(
                        box(
                            width = 10, status = "info", solidHeader = TRUE,
                            title = "Global Trend",
                            plotlyOutput("yearly_trend_by_continent")
                        ),
                        
                    )
            ),
            tabItem("trend",
                    
                    fluidPage(
                        sidebarLayout(
                            sidebarPanel(selectInput("continent", "Continent", 
                                                     unique(renewable_investment_long$Continent))),
                            mainPanel("")
                        ),
                        
                        box(
                            width = 10, status = "info", solidHeader = TRUE,
                            title = "Regional Trend",
                            plotlyOutput("yearly_trend_by_country")
                        ),
   
                    ))
            ,
            tabItem("opportunity",
                    
                    fluidPage(
                        sidebarLayout(
                            sidebarPanel(selectInput("continent2", "Continent", 
                                                     unique(renewable_investment_long$Continent)),
                                         sliderInput("year","Year",
                                                     min = unique(world_energy_2$Year)[1], 
                                                     max = unique(world_energy_2$Year)[length(unique(world_energy_2$Year))],
                                                     value = unique(world_energy_2$Year)[length(unique(world_energy_2$Year))],
                                                     step = 1,
                                                     sep = "",
                                                     ),
                                         
                                         radioButtons("bubble_option", "Adjust Bubble size by:", 
                                                     c("GDP","Renewable energy share of Total Energy Produced (%)"))
                                         
                                         ),
                            mainPanel("")
                        ),
                        
                        box(
                            width = 10, status = "info", solidHeader = TRUE,
                            title = "Opportunity",
                            plotlyOutput("yearly_accessibility")
                        ),
                        
       
                    ) 
                    
                    
            
            ),
            tabItem("summary",
                    box(width = 10, status = "info", solidHeader = TRUE,
                        title = "Summary",
                      p("Renewable energy is now responsible for 19.8% of energy generation in the US. With solar energy being a heavily invested and stable alternative to fossil fuels, it's becoming more and more apparently that solar energy is the future to energy production. Enter battery storage, which can be filled or charged when generation is high and power consumption is low, it's the best way to manage everchanging electric loads. When some of the electricity produced by the sun is put into storage, that electricity can be used whenever grid operators or individual homes need it, including after the sun has set. In this way, storage acts as an insurance policy for inclement weather. Solar energy and battery storage is an industrial and residential solution to the world's new energy demands.
Despite the pandemic, US investment in battery storage surged by almost 40% year-over-year in 2020, to 5.5 billion. Spending on grid-scale batteries rose by more than 60%, driven by the push for investments in renewables. The costs of battery storage systems reportedly continued to reduce substantially, by an average of 20%. This also helped drive the impressive resilience of grid-scale batteries, especially in the United States and China, installing over 1GW worth. According to Mercom Capital, US Venture capital funding went from 351 million to 994 million from Q4 2020 to Q1 2021. the rebound in large investments demonstrates a larger trend - investors are increasingly convinced that energy transitions are happening, with both start-ups and tried and true energy companies. These investments are encouraged by both advancements in the technology and increased progressive legislation demanding renewable energy storage and production systems."),
                      img(src="Battery Storage and Solar.png", width=450, height = 300),
                      img(src="Solar Farm.png", width=450, height = 300)
                    )
            )
        )
    )
    
  
  

)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    output$total_investment_dollar <- renderValueBox({
        
        valueBox(
            value = paste(formatC(total_renewable_investment_last, digits = 1, format = "f"),"Billion"),
            subtitle = glue("Total Renewable Investment in {last_year}"),
            icon = icon("dollar"),
            color = "yellow"
        )
    })
    
    
    
    output$year_of_year <- renderValueBox({
        
        valueBox(
            value = paste(formatC(round(yoy,1), digits = 1, format = "f"),"%"),
            subtitle = glue("Year over Year (from {last_year -1})"),
            icon = icon("performance1"),
            color = "green"
        )
    })
    
    
    
    output$yearly_trend_by_country <- renderPlotly({
        
      CONTINENT = input$continent
      
      REFERENCE_YEAR = last_year  # Reference year to arrange the stacking order
      
      subset <- renewable_investment_long %>% 
        filter(Continent==CONTINENT)
      
      rank <- subset %>% 
        filter(Year ==REFERENCE_YEAR) %>% 
        group_by(Country) %>% 
        summarize(`Renewable Investment` = sum(`Renewable Investment`)) %>% 
        arrange(`Renewable Investment`)
      
      subset$Country <- factor(subset$Country , levels=rank$Country)
      
      nb.cols <- length(unique(subset$Country))
      mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)
      
      p <- subset %>% 
        ggplot(aes(x = Year, y = `Renewable Investment`, 
                   fill = Country, group = Country, 
                   text = ifelse(`Renewable Investment` <= 1,
                                 glue("Year : {Year} <br> Country : {Country} <br> Renewable Investment: ${round(`Renewable Investment`*1000,1)} Million <br>"),
                                 glue("Year : {Year} <br> Country : {Country} <br> Renewable Investment: ${round(`Renewable Investment`,1)} Billion <br>"))), 
               col = "white") + 
        geom_area() +
        scale_y_continuous(labels = dollar_format()) +
        scale_x_continuous(breaks = seq(first_year,last_year,1)) + 
        theme_bw()+ 
        ylab("Renewable Investment (USD $ Billion)") +
        scale_fill_manual(values = mycolors)
      ggplotly(p, tooltip = "text")
        
    })
    
    output$yearly_trend_by_continent <- renderPlotly({
        
        
        yearly_investment_by_continent <- renewable_investment_long %>% 
            group_by(Continent, Year) %>% 
            summarize(`Renewable Investment` = sum(`Renewable Investment`)) 
        
        rank_by_continent <- yearly_investment_by_continent %>% 
            filter(Year == last_year) %>% 
            arrange(`Renewable Investment`)
        
        
        yearly_investment_by_continent$Continent <- factor(yearly_investment_by_continent$Continent , levels=rank_by_continent$Continent)
        
        
        p <- yearly_investment_by_continent %>% 
            ggplot(aes(x = Year, y= `Renewable Investment`, 
                       fill = Continent, group = Continent, 
                       text = ifelse(`Renewable Investment` <= 1,
                                     glue("Year : {Year} <br> Continent : {Continent} <br> Renewable Investment: ${round(`Renewable Investment`*1000,1)} Million <br>"),
                                     glue("Year : {Year} <br> Continent : {Continent} <br> Renewable Investment: ${round(`Renewable Investment`,1)} Billion <br>")))) +
            geom_bar(stat="identity", position = "dodge") +
            theme_bw(base_size = 14)+ 
            scale_x_continuous(breaks = seq(first_year,last_year,1)) + 
            scale_y_continuous(labels = dollar_format()) +
            ylab("Renewable Investment (USD $ Billion)") +
            scale_fill_brewer(palette = "Set3")
        
        ggplotly(p, tooltip = "text")
        
        
    })
    
    
    output$yearly_accessibility <- renderPlotly({
        
  
        nb.cols <- length(unique(world_energy_gdp$Country[world_energy_gdp$Continent== input$continent2]))
        mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)
        
        
        p <- world_energy_gdp %>% 
            filter(Continent == input$continent2) %>% 
            filter(Year == input$year) %>% 
            ggplot(aes(x = `Renewable electricity share of total electricity output (%)`, 
                       y = `Access to electricity (% of total population)`, 
                       col = Country, size = !! sym(input$bubble_option),
                       group = Country,
                       text = glue("Country: {Country} 
                                   Access to electricity (% of total population) : {round(`Access to electricity (% of total population)`,2)}% 
                                   Renewable electricity share of total electricity output (%) : {round(`Renewable electricity share of total electricity output (%)`,2)}%
                                   Renewable energy share of Total Energy Produced (%) : {round(`Renewable energy share of Total Energy Produced (%)`,2)}%
                                   GDP : {dollar(GDP)} per Capita"))) +
            geom_point() +
            theme_bw() + 
            guides(fill=guide_legend(title="Country")) +
            scale_color_manual(values = mycolors)
        
        ggplotly(p, tooltip = "text")
        
    })
    
    
    # output$yearly_gdp <- renderPlotly({
    #   
    #   
    #   
    #   nb.cols <- length(unique(world_energy_gdp$Country[world_energy_gdp$Continent== input$continent2]))
    #   mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)
    #   
    #   
    #   rank_by_continent <- world_energy_gdp %>% 
    #     filter(Year == input$year) %>% 
    #     arrange(desc(`GDP`))
    #   
    #   
    #   world_energy_gdp$Country <- factor(world_energy_gdp$Country , levels=rank_by_continent$Country)
    #   
    #   
    #   
    #   p <- world_energy_gdp %>% 
    #     filter(Continent == input$continent2) %>% 
    #     filter(Year == input$year) %>% 
    #     ggplot(aes(x= Country, y= GDP, fill = Country, text = glue("Country : {Country} <br> Year: {Year} <br> GDP: {dollar(round(GDP,2))} per Capita")))+
    #     geom_bar(stat = "identity", position = "dodge")+
    #     scale_y_continuous(labels = dollar_format()) +
    #     theme_bw() + 
    #     theme(axis.text.x = element_text(angle = 90)) + 
    #     guides(fill=guide_legend(title="Country")) +
    #     scale_fill_manual(values = mycolors)
    #   
    #   ggplotly(p, tooltip = "text")
    #   
    # 
    # })
    # 
    # 
    
    
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
