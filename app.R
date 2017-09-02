

library(shiny)
library(shinyjs)
library(zoo)
library(ggplot2)
library(scales)
library(readr)
library(dplyr)
library(tibble)
library(reshape2)
library(Cairo)
options(shiny.usecairo=T)

# UI ----------------------------------------------------------------------


appCSS <- "#loading-content {
  position: absolute;
  background: #FFF;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #000000;
}"

mycss <- "#plot-container {
  position: relative;
  }
  #loading-spinner {
  position: absolute;
  left: 50%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
  }
  #plot.recalculating {
  z-index: -2;
}"

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    tags$style(HTML(mycss))),
  useShinyjs(),
  inlineCSS(appCSS),
  
  div(
    id = "loading-content",
    h2("Loading...")
  ),
  
  hidden(
    div(
      id = "app-content",
  
  column(2),
  column(8,
         
         fluidRow(
           p("*Updated every few days."),
              
              p("In a", tags$a(href="https://evanodell.com/blog/2017/02/06/cycling-vs-oyster/", "blog post"), "in February I analysed how much money I was saving by cycling to work instead of using a monthly Oyster transit pass. When I wrote that blog I had spent almost £20 more on my bike and pay-as-you-go transit pass than I would have if I bought a monthly transit pass. As of 29 April 2017, less than 3 months after that blog, and despite needing a new rear wheel, I broke even, and I've been updating my data every few days, and have now built another", tags$a(href="https://shiny.rstudio.com/", "Shiny"), "app to monitor my spending on my bike and on transit."),
              
              p("I include all spending directly on my bike, including the cost of the bike, accessories, spare parts, tools and maintenance. I also include non-bike costs that are the result of cycling, primarily clothing. For instance, I have bought a couple pairs of commuter trousers for cycling and include that spending in my calculations, less £40 to represent the price of a standard pair of men's trousers, on the basis that I would have had to buy new trousers anyways."),
              
              p("You can see in the second time series plot that since writing the blog post in February my Oyster spending has dropped off somewhat. Since analysing how much I was cycling, and how much I was spending on transit, I've become much more dedicated to riding places, no longer taking the bus or the tube if I'm feeling a little bit lazy."), 
      
           uiOutput("slider"),
           em(h4(textOutput("last_update"))),
           em(h4(textOutput("savings"))),
           h4("Total Spending and Combined Spending:"),
           div(id = "plot-container",
               tags$img(src = "spinner.gif",
                        id = "loading-spinner"),
               plotOutput("p1")),
           em(textOutput("p1_text"))),
         
         fluidRow(
           h4("Time Series of Spending on Pay-As-You-Go Oyster and Cycling:"),
           div(id = "plot-container",
               tags$img(src = "spinner.gif",
                        id = "loading-spinner"),
               plotOutput("p2")),
           em(textOutput("p2_text"))),
         
         fluidRow(
           h4("Cumulative Spending in Each Category:"),
           div(id = "plot-container",
               tags$img(src = "spinner.gif",
                        id = "loading-spinner"),
               plotOutput("p3")),
           h4("Rolling average bicycle cost per day:"),
           div(id = "plot-container",
               tags$img(src = "spinner.gif",
                        id = "loading-spinner"),
               plotOutput("p4")),
           em(textOutput("p3_text"))),
         
         fluidRow(
           br(),
           p(textOutput("other_options_text")),
           p(textOutput("fines_text")),
           
           p("There are a number of obstacles to an exact cost comparison of cycling and public transit. For example, if I go on holiday for a couple weeks, I might time my travelcard renewal so I'm not paying anything while not in London. Moreover, there are the intangible benefits of exercise and shorter commutes from cycling, compared to the convenience and low effort required of public transit, and any purely financial comparison misses those factors.")
           )
         ),
  column(2))))

# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  
  bike_data_full <- read_csv("cycling_oyster_data.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))

  bike_data_full$mon_oyster_per_day <- ifelse(bike_data_full$Date <= "2017-01-02", 124.50/30, 126.80/30)

  bike_data_full$week_oyster_per_day <- ifelse(bike_data_full$Date <= "2017-01-02", 32.4/7, 33/7)

  bike_data_full$annual_oyster_per_day <- ifelse(bike_data_full$Date <= "2017-01-02", 1296/365, 1320/365)

  # Slider ------------------------------------------------------------------
  
  output$slider <- renderUI({
    sliderInput("inslider","Adjust date range:", min   = as.Date("2016-06-30"), 
                max   = max(bike_data_full$Date),
                value = max(bike_data_full$Date))
    })
  
  bike_data_subset <- reactive({
    bike_data_full[bike_data_full$Date  <= input$inslider,]
  })
  
  pound <- function(x) {
    paste0("£",format(x, big.mark = ",",
                      decimal.mark = ".",
                      trim = TRUE, scientific = FALSE))
  }

  output$last_update <- renderText(paste0("Last Updated: ", format(max(bike_data_full$Date),format="%d %B %Y")))
  
  output$p1_text <- renderText({
    
    bike_data <- bike_data_subset()
    
    paste0("The red and green bars are total spending on my bike and related accessories and my pay-as-you-go Oyster spending, respectively. The blue bar is the combined total of bicycle and pay-as-you-go spending, and the purple bar is the hypothetical total spending of monthly Travelcards covering ", as.character(max(bike_data$Date) - min(bike_data$Date)), " days, from 30 June 2016 to ", format(max(bike_data$Date),format="%d %B %Y"),".")
    
    })

  output$p2_text <- renderText({
    
    bike_data <- bike_data_subset()
    
    comparison <- sum(bike_data$Bike, bike_data$Oyster, (nrow(bike_data)/365)*60) - sum(bike_data$mon_oyster_per_day)
    
    if(comparison > 0) {
      compare <- "more"
    } else {
      compare <- "less"
    }
    
    #dailys <- sprintf("%.2f",round(abs(((sum(bike_data$Oyster)/nrow(bike_data)) + bike_average)-4.23),2))
    
   paste0("The green horizontal line represents the average daily cost of a monthly zone 1-2 Travelcard in London, £124.50 in 2016 and £126.80 in 2017. The burgundy horizontal line represents the average daily cost of my bicycle and accessories (£",sprintf("%.2f", round(sum(bike_data$Bike,(nrow(bike_data)/365)*60)/nrow(bike_data), 2)) ,") per day. The light blue line is a rolling monthly average of daily pay-as-you-go Oyster spending, and the light red line is pay-as-you-go Oyster spending combined with average daily bike costs. The average cost-per-day of my pay-as-you-go Oyster card is £", sprintf("%.2f",round((sum(bike_data$Oyster)/nrow(bike_data)),2)), ", which combined with bike spending means I have spent an average of £", sprintf("%.2f",abs(round(comparison/nrow(bike_data),2))), " per day ", compare, " than I would using a monthly travelcard.")
    
    })
  
  output$p3_text <- renderText({
    
    bike_data <- bike_data_subset()
    
    paste0("Cumulative spending in each category over ", as.character(max(bike_data$Date) - min(bike_data$Date)), " days, from 30 June 2016 to ", format(max(bike_data$Date),format="%d %B %Y"),", and a 30-day rolling average of daily bicycle costs.")
    
    })
  
  output$other_options_text <- renderText({
    
    bike_data <- bike_data_subset()
    
    savings_week <- sprintf("%.2f", round(sum(bike_data$week_oyster_per_day) - (nrow(bike_data) * mean(bike_data$mon_oyster_per_day)),2))
    
    annual_win_loss <- round(mean(bike_data$annual_oyster_per_day) - (nrow(bike_data) * mean(bike_data$mon_oyster_per_day)),2)
    
    if(annual_win_loss>0) {
      awl <- "saved £"
    } else {
      awl <- "lost £"
    }
    
    savings_annual <- paste0(awl, sprintf("%.2f", abs(round(sum(bike_data$annual_oyster_per_day) - (sum(bike_data$Bike) + sum(bike_data$Oyster)),2))))
    
    
    other_options_text <- paste0("It is worth noting other options for paying for transit passes. If buying weekly Travelcards, assuming I purchased one every week, I would have spent £", sprintf("%.2f",round(sum(bike_data$week_oyster_per_day),2)), " over the same period. Using an annual Travelcard would cost, pro-rated over this time period, £", sprintf("%.2f",round(sum(bike_data$annual_oyster_per_day),2)), ". Compared to a weekly oyster card, cycling has saved me £", savings_week, ", while I have ",savings_annual," compared to using an annual Travelcard.")
    
    print(other_options_text)
    
    })
  
  output$fines_text <- renderText({
    
    bike_data <- bike_data_subset()
    
    bike_locker_sum <-(nrow(bike_data)/365) * 60 ###Total bike locker spend, pro-rated
    
    current_total = round(sum(bike_data$Oyster) + sum(bike_data$Bike, bike_locker_sum),2)
    Travelcard_total = round(sum(bike_data$mon_oyster_per_day),2)
    
    fines_text <- paste0("I should also note that my total oyster spending includes fines for not tapping in or out correctly, and trips outside of zones 1-2, which would also be charged if I was using a travelcard. Accounting for the £", sprintf("%.2f", round(sum(bike_data$Fines))), " in fines and travel outside zone 2 that I presumably would have paid regardless, my savings are £", (Travelcard_total - current_total+sum(bike_data$Fines)),".")
    
    print(fines_text)
    
    })
  
  output$savings <- renderText({
    
    bike_data <- bike_data_subset()
    
    bike_locker_sum <-(nrow(bike_data)/365) * 60
    
    current_total <- sum(bike_data$Oyster, bike_data$Bike, bike_locker_sum)
    
    Travelcard_total <- sum(bike_data$mon_oyster_per_day)
    
    if(Travelcard_total - current_total > 0) {
      totsav <- "savings "
    } else {
      totsav <- "losses "
    }
    
    savings <- paste0("Total ",totsav,  "from cycling instead of using public transit: ", "£",sprintf("%.2f", abs(round(Travelcard_total - current_total, 2))))
    
    print(savings)
    
    })
  
  
# p1 ----------------------------------------------------------------------
  output$p1 <- renderPlot({
    
    bike_data <- bike_data_subset()
    
    #bike_average <- mean(bike_data$Bike) + (()
    
    bike_average <- mean(bike_data$Bike) + (((nrow(bike_data)/365) * 60)/nrow(bike_data))
    
    #days_covered <- as.character(max(bike_data$Date) - min(bike_data$Date))
  
    travel_summary <- melt(data.frame(Bike_Total = sum(bike_data$Bike, (nrow(bike_data)/365) * 60),
                                      Oyster_Total = sum(bike_data$Oyster),
                                      Combined_Total = sum(bike_data$Oyster, bike_data$Bike,
                                                          (nrow(bike_data)/365) * 60),
                                      Hypothetical_Travelcard_Total = sum(bike_data$mon_oyster_per_day)))
    
    travel_summary$variable <- gsub("_", " ", travel_summary$variable)

    travel_summary$variable <- factor(travel_summary$variable, levels=c("Bike Total", "Oyster Total", "Combined Total", "Hypothetical Travelcard Total"))

p1 <- ggplot(travel_summary, aes(x=variable, y=value, fill=variable, label = value)) +
      geom_bar(stat = "identity", position = position_dodge(width=0.5)) +
      geom_text(aes(y = value + 0.1, 
                    label=paste0("£", sprintf("%.2f", round(value,2)))),
                position = position_dodge(0.9),
                vjust = -0.25,
                fontface = "bold",
                size = 5) +
      scale_y_continuous(labels = pound, name=paste0("Total Spending from \n",
                                     format(min(bike_data$Date),format="%d %B %Y"),
                                     " to ",
                                     format(max(bike_data$Date), format="%d %B %Y"))) +
      scale_x_discrete(name="Type of Spending") +
      theme(legend.position = "bottom"
            , text=element_text(size=14),
            legend.text=element_text(size=14),
            axis.text.y = element_text(size=14)) +
      scale_fill_discrete("")

    print(p1)
    
  })
  
# p2 ----------------------------------------------------------------------
  output$p2 <- renderPlot({
    
    bike_data <- bike_data_subset() 
    
    bike_average <- mean(bike_data$Bike) + (((nrow(bike_data)/365) * 60)/nrow(bike_data))
    
    oyster_roll_gg <- as.tibble(
      rollapply(zoo(bike_data$Oyster, order.by=bike_data$Date),7, mean, align="right")
      )
    
    oyster_roll_gg$Date <- as.Date(row.names(oyster_roll_gg))
    
    names(oyster_roll_gg)[1] <- "Oyster_Charge"
    
    oyster_roll_gg$Bike_plus_Oyster <- mean(bike_data[["Bike"]]) + oyster_roll_gg$Oyster_Charge
    
    oyster_roll_gg<- melt(oyster_roll_gg, id="Date")
    
    ## Recoding oyster_roll_gg$variable into oyster_roll_gg$variable
    oyster_roll_gg$variable <- as.character(oyster_roll_gg$variable)
    oyster_roll_gg$variable[oyster_roll_gg$variable == "Oyster_Charge"] <- "PAYG Oyster Spending"
    oyster_roll_gg$variable[oyster_roll_gg$variable == "Bike_plus_Oyster"] <- "Oyster Plus Bike Spending"
    oyster_roll_gg$variable <- factor(oyster_roll_gg$variable)
    
    p2 <- ggplot(oyster_roll_gg, aes(x=Date)) +
      geom_hline(aes(yintercept=bike_average, linetype="Bicycle Cost-Per-Day"),
                 col = "#b5000e",
                 size=1, 
                 show.legend = TRUE) +
      geom_hline(aes(yintercept=mean(bike_data$mon_oyster_per_day),
                     linetype="Travelcard Cost-Per-Day"),
                 col = "#01e245",
                 size=1,
                 show.legend = FALSE) +
      scale_linetype_manual(values = c(2, 2),
                            guide = guide_legend(title = NULL,
                                                 nrow=2,
                                                 override.aes = list(color = c("#b5000e", "#01e245")))) +
      geom_line(aes(y=value, col = variable), size=1) +
      scale_color_discrete("") + 
      scale_x_date(date_breaks = "4 weeks") +
      scale_y_continuous(name="Average charge over previous 7 days", labels = pound) +
      guides(col = guide_legend(nrow = 2, bycol = TRUE)) +
      geom_text(aes(x = max(Date),
                    y = bike_average,
                    hjust= "right",
                    vjust = 1.5,
                    label = paste0("Bicycle Cost Per Day (£",
                                   sprintf("%.2f", round(bike_average,2)),")")), size=5.5) +
      geom_text(aes(x = max(Date), 
                    y = mean(bike_data$mon_oyster_per_day), 
                    hjust= "right", 
                    vjust = 1.5,
                    label = paste0("Monthly Zone 1-2 Travelcard (£",
                                   sprintf("%.2f", round(mean(bike_data$mon_oyster_per_day),2)),")")), 
                size=5.5)+
      theme(legend.position = "bottom", 
            axis.text.x = element_text(angle = 30, hjust = 1, size=14),  
            legend.text=element_text(size=14), 
            axis.text.y = element_text(size=14))
    
    print(p2)
    
  })
  
  output$p3 <- renderPlot({
    
    bike_data <- bike_data_subset()
  
    bike_melt <- melt(bike_data[c(1:3)], id = c("Date"))
    
    bike_melt2 <- bike_melt %>% group_by(variable) %>% arrange(Date) %>% mutate(spending = cumsum(value))
    
    # p3 ----------------------------------------------------------------------
    
    p3 <- ggplot(bike_melt2) + geom_line(aes(x=Date,y=spending, col = variable), size=1) +
      scale_y_continuous(name = "Cumulative Spending", 
                         labels = pound) + 
      scale_x_date(date_breaks = "4 weeks") + 
      scale_color_manual(values = c("#01e245","#b5000e"), 
                         labels = c("Pay As You Go Oyster Spending","Bike Spending"), 
                         name="") + 
      theme(legend.position = "bottom", 
            axis.text.x = element_text(angle = 30, hjust = 1, size=14), 
            legend.text=element_text(size=14), 
            axis.text.y = element_text(size=14))
    
    print(p3)
  
  })
  
# p4 ----------------------------------------------------------------------
  output$p4 <- renderPlot({
    
    bike_data <- bike_data_subset()
    
    bike_data$cumsum <- cumsum(bike_data$Bike)/as.numeric(bike_data$Date - as.Date("2016-06-29"))
    
    bike_roll_gg <- tibble::as_tibble(rollapply(zoo(bike_data$cumsum, order.by=bike_data$Date), 30, mean))
    
    bike_roll_gg$Date <- as.Date(row.names(bike_roll_gg))
    
    names(bike_roll_gg)[1] <- "spending"
    
    p4 <- ggplot(bike_roll_gg) + geom_line(aes(x=Date,y=spending), col = "#b5000e", size=1) +
      scale_y_sqrt(name = "Average bicycle cost per day over the previous 30 days", 
                   labels = pound,
                   breaks=c(0,2,4,6,8,10,20,30,40,50)) + 
      scale_x_date(date_breaks = "4 weeks") + 
      scale_color_manual(values = c("#b5000e"), 
                         labels = c("Bike Spending")) + 
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 30, hjust = 1, size=14),
            legend.text=element_text(size=14),
            axis.text.y = element_text(size=14))
    
    print(p4)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


