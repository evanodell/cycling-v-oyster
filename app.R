

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

appCSS <- "
#loading-content {
  position: absolute;
  background: #FFF;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #000000;
}
"

mycss <- "
#plot-container {
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
}
"

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
           em(h4(textOutput("last_update"))),
           h4("Total Spending and Combined Spending:"),
           div(id = "plot-container",
               tags$img(src = "spinner.gif",
                        id = "loading-spinner"),
               plotOutput("p1")),
           textOutput("p1_text")),
         
         fluidRow(
           h4("Time Series of Spending on Pay-As-You-Go Oyster and Cycling:"),
           div(id = "plot-container",
               tags$img(src = "spinner.gif",
                        id = "loading-spinner"),
               plotOutput("p2")),
           textOutput("p2_text")),
         
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
           textOutput("p3_text")),
         
         fluidRow(
           br(),
           textOutput("other_options_text"),
           em(h4(textOutput("savings"))))
  ),
  column(2))))

server <- function(input, output) {
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  
  bike_data <- read_csv("cycling_oyster_data.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))

  
  pound <- function(x) {
    paste0("£",format(x, big.mark = ",",
                      decimal.mark = ".",
                      trim = TRUE, scientific = FALSE))
  }
  
  "Average Monthly Travelcard Cost per Day" <- 126.80/30
  
  oyster_card <- 126.80/30
  
  payg_oyster_card <- sum(bike_data$Oyster)/nrow(bike_data)
  
  bike_locker_avg <- ((nrow(bike_data)/365) * 60)/nrow(bike_data) ## Daily cost of bike locker, pro-rated
  
  bike_locker_sum <- (nrow(bike_data)/365) * 60 ###Total bike locker spend, pro-rated
  
  "Average Bicycle Cost per Day" <- mean(bike_data[["Bike"]]) + bike_locker_avg
  
  bike_average <- mean(bike_data[["Bike"]]) + bike_locker_avg 
  
  bike_avg <- sprintf("%.2f", round(bike_average, 2))
  
  annual_oyster <- sprintf("%.2f", round((1320/365)*nrow(bike_data),2))
  
  week_oyster <- sprintf("%.2f", round((33/7)*nrow(bike_data),2))
  
  days_covered <- as.character(max(bike_data$Date) - min(bike_data$Date))
  
  travel_summary <- data.frame(bike_total = round(sum(bike_data[["Bike"]], bike_locker_sum),2),
                               oyster_total = round(sum(bike_data[["Oyster"]]),2),
                               current_total = round(sum(bike_data[["Oyster"]]) + sum(bike_data[["Bike"]], bike_locker_sum),2),
                               Travelcard_total = round(nrow(bike_data) * oyster_card,2)
  )
  
  total_savings <- paste0("£",sprintf("%.2f", abs(round(travel_summary$Travelcard_total - travel_summary$current_total, 2))))
  
  tot_savings_compare <- round(travel_summary$Travelcard_total - travel_summary$current_total, 2)
  
  travel_summary$class <- NA
  
  travel_summary <- melt(travel_summary, id = "class")
  
  travel_summary$class <- NULL
  travel_summary$variable <- as.character(travel_summary$variable)
  travel_summary$variable[travel_summary$variable == "bike_total"] <- "Bike Total"
  travel_summary$variable[travel_summary$variable == "oyster_total"] <- "Oyster Total"
  travel_summary$variable[travel_summary$variable == "current_total"] <- "Combined Total"
  travel_summary$variable[travel_summary$variable == "Travelcard_total"] <- "Hypothetical Travelcard Total"
  travel_summary$variable <- factor(travel_summary$variable, levels=c("Bike Total", "Oyster Total", "Combined Total", "Hypothetical Travelcard Total"))
  
  oyster_ts <- zoo(bike_data$Oyster, order.by=bike_data$Date)
  
  oyster_roll <- rollapply(oyster_ts, 7, mean, align="right")
  
  oyster_roll_gg <- as.tibble(oyster_roll)
  
  oyster_roll_gg$Date <- as.Date(row.names(oyster_roll_gg))
  
  names(oyster_roll_gg)[1] <- "Oyster_Charge"
  
  oyster_roll_gg$Bike_plus_Oyster <- mean(bike_data[["Bike"]]) + oyster_roll_gg$Oyster_Charge
  
  oyster_roll_gg<- melt(oyster_roll_gg, id="Date")
  
  ## Recoding oyster_roll_gg$variable into oyster_roll_gg$variable
  oyster_roll_gg$variable <- as.character(oyster_roll_gg$variable)
  oyster_roll_gg$variable[oyster_roll_gg$variable == "Oyster_Charge"] <- "PAYG Oyster Spending"
  oyster_roll_gg$variable[oyster_roll_gg$variable == "Bike_plus_Oyster"] <- "Oyster Plus Bike Spending"
  oyster_roll_gg$variable <- factor(oyster_roll_gg$variable)
  
  
  output$last_update <- renderText(paste0("Last Updated: ", format(max(bike_data$Date),format="%d %B %Y")))
  
  savings_week <- sprintf("%.2f", round((33/7)*nrow(bike_data) - (nrow(bike_data) * oyster_card),2))
  
  annual_win_loss <- round((1320/365)*nrow(bike_data) - (nrow(bike_data) * oyster_card),2)
  
  if(annual_win_loss>0) {
    awl <- "saved £"
  } else {
    awl <- "lost £"
  }
  
  savings_annual <- paste0(awl, sprintf("%.2f", abs(round((1320/365)*nrow(bike_data) - (nrow(bike_data) * oyster_card),2))))
  
  output$p1_text <- renderText(paste0("The red and green bars are total spending on my bike and related accessories and my pay-as-you-go Oyster spending, respectively. The blue bar is the combined total of bicycle and pay-as-you-go spending, and the purple bar is the hypothetical total spending of monthly Travelcards covering ", days_covered, " days, from 30 June 2016 to ", format(max(bike_data$Date),format="%d %B %Y"),"."))
  
  dailys <- round((payg_oyster_card + bike_average)-4.23,2)
  
  if(dailys > 0) {
    compare <- "more"
  } else {
    compare <- "less"
  }
  
  dailys <- abs(dailys)
  
  output$p2_text <- renderText(paste0("The green horizontal line represents the average daily cost of a monthly zone 1-2 Travelcard in London (£4.23 per day), and the burgundy horizontal line represents the average daily cost of my bicycle and accessories (£",bike_avg ,") per day. The light blue line is a rolling monthly average of daily pay-as-you-go Oyster spending, and the light red line is pay-as-you-go Oyster spending combined with average daily bike costs. The average cost-per-day of my pay-as-you-go Oyster card is £", round(payg_oyster_card,2), ", which combined with bike spending means I have spent an average of £", dailys, " per day ", compare, " than I would using a monthly travelcard."))
  
  output$p3_text <- renderText(paste0("Cumulative spending in each category over ", days_covered, " days, from 30 June 2016 to ", format(max(bike_data$Date),format="%d %B %Y"),", and a 7-day rolling average of daily bicycle costs."))
  

  output$other_options_text <- renderText(paste0("It is worth noting other options for paying for transit passes. If buying weekly Travelcards, assuming I purchased one every week, I would have spent £", week_oyster, " over the same period. Using an annual Travelcard would cost, pro-rated over this time period, £", annual_oyster, ". If comparing to a weekly oyster card, cycling has saved me £", savings_week, ", while compared to a pro-rated annual Travelcard I have ",savings_annual,". I should also note that my total oyster spending includes fines for not tapping in or out correctly, and trips outside of zones 1-2, which would also be charged if I was using a travelcard."))
  
  if(tot_savings_compare > 0) {
    totsav <- "savings "
  } else {
    totsav <- "losses "
  }

  output$savings <- renderText(paste0("Total ",totsav,  "from cycling instead of using public transit: ", total_savings))
  
  
  output$p1 <- renderPlot({
    
    p1 <- ggplot(travel_summary, aes(x=variable, y=value, fill=variable, label = value)) +
      geom_bar(stat = "identity", position = position_dodge(width=0.5)) +
      geom_text(aes(y = value + 0.1, label=paste0("£", sprintf("%.2f", round(value,2)))), position = position_dodge(0.9), vjust = -0.25, fontface = "bold") +
      scale_y_continuous(name=paste0("Total Spending from ", format(min(bike_data$Date),format="%d %B %Y"), " to ", format(max(bike_data$Date),format="%d %B %Y")), labels = pound) +
      scale_x_discrete(name="Type of Spending") +
      theme(legend.position = "bottom") +
      scale_fill_discrete("")

    print(p1)
    
  })
  
  output$p2 <- renderPlot({
    
    p2 <- ggplot(oyster_roll_gg, aes(x=Date)) +
      geom_hline(aes(yintercept=bike_average, linetype="Bicycle Cost Cost-Per-Day"), col = "#b5000e", size=1) +
      geom_hline(aes(yintercept=oyster_card,linetype="Monthly Travelcard Cost-Per-Day"), col = "#01e245", size=1) +
      scale_linetype_manual(values = c(2, 2), guide = guide_legend(title = NULL, override.aes = list(color = c("#b5000e", "#01e245")))) +
      geom_line(aes(y=value, col = variable), size=1) +
      scale_color_discrete("") + scale_x_date(date_breaks = "2 weeks") +
      scale_y_continuous(name="Average charge over previous 7 days", labels = pound) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1)) +
      guides(col = guide_legend(ncol = 2, bycol = FALSE)) +
      geom_text(aes(label = paste0("Bicycle Cost Per Day (£",sprintf("%.2f", round(bike_average,2)),")"), x = max(Date), y = bike_average, hjust= "right", vjust = 1.5), size=5) +
      geom_text(aes(label = paste0("Monthly Zone 1-2 Travelcard (£",sprintf("%.2f", round(oyster_card,2)),")"), x = max(Date), y = oyster_card, hjust= "right", vjust = 1.5), size=5)
    
    print(p2)
    
  })
  
  output$p3 <- renderPlot({
  
    bike_melt <- melt(bike_data[c(1:3)], id = c("Date"))
    
    bike_melt2 <- bike_melt %>% group_by(variable) %>% arrange(Date) %>% mutate(spending = cumsum(value))
    
    p3 <- ggplot(bike_melt2) + geom_line(aes(x=Date,y=spending, col = variable), size=1) +
      scale_y_continuous(name = "Cumulative Spending", labels = pound) + 
      scale_x_date(date_breaks = "2 weeks") + 
      scale_color_manual(values = c("#01e245","#b5000e"), labels = c("Pay As You Go Oyster Spending","Bike Spending"), name="") + 
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1))
    
    print(p3)
  
  })
  
  
  output$p4 <- renderPlot({
    
    bike_data$cumsum <- cumsum(bike_data$Bike)/as.numeric(bike_data$Date - as.Date("2016-06-29"))
    
    bike_ts <- zoo(bike_data$cumsum, order.by=bike_data$Date)
    
    bike_roll <- rollapply(bike_ts, 7, mean)
    
    bike_roll_gg <- tibble::as.tibble(bike_roll)
    
    bike_roll_gg$Date <- as.Date(row.names(bike_roll_gg))
    
    names(bike_roll_gg)[1] <- "spending"
    
    p4 <- ggplot(bike_roll_gg) + geom_line(aes(x=Date,y=spending), col = "#b5000e", size=1) +
      scale_y_sqrt(name = "Average bicycle cost per day over previous 7 days", labels = pound, breaks=c(0,2,4,6,8,10,20,30,40,50)) + 
      scale_x_date(date_breaks = "2 weeks") + 
      scale_color_manual(values = c("#b5000e"), labels = c("Bike Spending")) + 
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1))

    print(p4)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



