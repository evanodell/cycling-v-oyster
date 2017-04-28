

library(shiny)

ui <- fluidPage(

        fluidRow(
          h4("Total Spending"),
          plotOutput("p2",width = "75%"),
          em("The green horizontal line represents the average daily cost of a monthly zone 1-2 travelcard in London (£4.23 per day), and the burgundy horizontal line represents the average daily cost of my bicycle and accessories (£2.06 per day). The light blue line is a rolling monthly average of daily pay-as-you-go Oyster spending, and the light red line is pay-as-you-go Oyster spending combined with daily bike costs.")),
   fluidRow(
     h4("Time Series of Spending on Pay-As-You-Go Oyster and on Cycling"),
        plotOutput("p1"),
     em(textOutput("last_update"))),
   
   fluidRow(
        h3(textOutput("savings")))
   )

server <- function(input, output) {
  
  library(zoo)
  library(ggplot2)
  library(data.table)
  library(scales)
  library(grid)
  library(readr)
  
  bike_data <- read_csv("cycling_oyster_data.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))
  
  output$last_update <- renderText(paste0("The red and green bars are total spending on my bike and related accessories and my pay-as-you-go Oyster spending, respectively. The blue bar is the combined total of bicycle and pay-as-you-go spending, and the purple bar is the hypothetical total spending of monthly travelcards covering 2016-06-30 to ",max(bike_data$Date)))
  
  pound <- function(x) {
    paste0("£",format(x, big.mark = " ",
                      decimal.mark = ",",
                      trim = TRUE, scientific = FALSE))
  }
  
  #summary(bike_data)
  
  "Average Monthly Travelcard Cost per Day" <- 126.80/30  ##Once with label name, once with short variable name, because sometimes I'm lazy and ignore best practices. Don't judge me, you do it too.
  
  oyster_card <- 126.80/30
  
  bike_locker_avg <- ((nrow(bike_data)/365) * 60)/nrow(bike_data) ## Daily cost of bike locker, pro-rated
  
  bike_locker_sum <- (nrow(bike_data)/365) * 60 ###Total bike locker spend, pro-rated
  
  "Average Bicycle Cost per Day" <- mean(bike_data[["Bike"]]) + bike_locker_avg ##Once with label name, once with short variable name, because sometimes I'm lazy and ignore best practices. Don't judge me, you do it too.
  
  bike_average <- mean(bike_data[["Bike"]]) + bike_locker_avg ##Once with label name, once with short variable name, because sometimes I'm lazy and ignore best practices. Don't judge me, you do it too.
  
  travel_summary <- data.frame(bike_total = round(sum(bike_data[["Bike"]], bike_locker_sum),2),
                               oyster_total = round(sum(bike_data[["Oyster"]]),2),
                               current_total = round(sum(bike_data[["Oyster"]]) + sum(bike_data[["Bike"]], bike_locker_sum),2),
                               travelcard_total = round(nrow(bike_data) * oyster_card,2)
  )
  
  total_savings <- paste0("Difference: £", sprintf("%.2f", round(travel_summary$travelcard_total - travel_summary$current_total, 2)))
  
  output$savings <- renderText(paste0("Total savings from cycling instead of using public transit: ", total_savings))
  
  travel_summary$class <- NA
  
  travel_summary <- melt(travel_summary, id = "class")
  
  travel_summary$class <- NULL
  
  ## Recoding travel_summary$variable into travel_summary$variable
  ## Reordering travel_summary$variable
  
  travel_summary$variable <- as.character(travel_summary$variable)
  travel_summary$variable[travel_summary$variable == "bike_total"] <- "Bike Total"
  travel_summary$variable[travel_summary$variable == "oyster_total"] <- "Oyster Total"
  travel_summary$variable[travel_summary$variable == "current_total"] <- "Current Total"
  travel_summary$variable[travel_summary$variable == "travelcard_total"] <- "Hypothetical Travelcard Total"
  travel_summary$variable <- factor(travel_summary$variable, levels=c("Bike Total", "Oyster Total", "Current Total", "Hypothetical Travelcard Total"))
  
  oyster_ts <- zoo(bike_data$Oyster, order.by=bike_data$Date)
  
  oyster_roll <- rollapply(oyster_ts, 7, mean)
  
  oyster_roll_gg <- as.data.frame(oyster_roll)
  
  setDT(oyster_roll_gg, keep.rownames = TRUE)[]
  
  names(oyster_roll_gg)[1] <- "Date"
  names(oyster_roll_gg)[2] <- "Oyster_Charge"
  oyster_roll_gg$Date <- as.Date(oyster_roll_gg$Date)
  
  oyster_roll_gg$Bike_plus_Oyster <- mean(bike_data[["Bike"]]) + oyster_roll_gg$Oyster_Charge
  
  oyster_roll_gg<- melt(oyster_roll_gg, id="Date")
  
  ## Recoding oyster_roll_gg$variable into oyster_roll_gg$variable
  oyster_roll_gg$variable <- as.character(oyster_roll_gg$variable)
  oyster_roll_gg$variable[oyster_roll_gg$variable == "Oyster_Charge"] <- "Pay as You Go Oyster Spending"
  oyster_roll_gg$variable[oyster_roll_gg$variable == "Bike_plus_Oyster"] <- "Oyster Spending Plus Bike Average"
  oyster_roll_gg$variable <- factor(oyster_roll_gg$variable)
  
  output$p1 <- renderPlot({
  
  p1 <- ggplot(oyster_roll_gg, aes(x=Date)) +
    geom_hline(aes(yintercept=bike_average, linetype="Average Bicycle Cost per Day"), colour = "#b5000e", size=1) +
    geom_hline(aes(yintercept=oyster_card,linetype="Average Monthly Travelcard Cost per Day"), color = "#01e245", size=1) +
    scale_linetype_manual(values = c(2, 2),
                          guide = guide_legend(title = NULL, override.aes = list(color = c("#b5000e", "#01e245")))) +
    geom_line(aes(y=value, col = variable), size=1) +
    scale_color_discrete("") +
    scale_x_date(date_breaks = "1 month") +
    scale_y_continuous(name="Charge over previous 7 days", labels = pound) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1)) +
    ggtitle("Cycling vs Travelcard") +
    guides(col = guide_legend(ncol = 2, bycol = FALSE)) +
    geom_text(aes(label = "Bicycle Cost", x = max(Date), y = bike_average, hjust= "right", vjust = 1)) +
    geom_text(aes(label = "Monthly Zone 1-2 Travelcard", x = max(Date), y = oyster_card, hjust= "right", vjust = 1))
  
  print(p1)
  
  })
  
  output$p2 <- renderPlot({
  
  p2 <- ggplot(travel_summary, aes(x=variable, y=value, fill=variable, label = value)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) +
    geom_text(aes(y = value + 0.1, label=paste0("£", value)), position = position_dodge(0.9), vjust = -0.25, fontface = "bold") +
    scale_y_continuous(name="Total Spending from 2016-06-30 to 2017-02-03", labels = pound) +
    scale_x_discrete(name="Type of Spending") +
    ggtitle("Total Cost of Different Transport Modes") +
    theme(legend.position = "bottom") +
    #annotate("text", x = 1.5, y = 750, label=paste0("Difference: £", total_savings), fontface = "bold") +
    #annotate("rect", xmin = 1.1, xmax = 1.9, ymin = 720, ymax = 780, alpha = .2, colour = "blue", fill = "blue") +
    scale_fill_discrete("")
  
  print(p2)
  
  })


}

# Run the application 
shinyApp(ui = ui, server = server)

