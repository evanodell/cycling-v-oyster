

library(readr)
library(zoo)
library(ggplot2)
library(data.table)
library(scales)
library(grid)
library(tibble)

library(extrafont)
font_import()


bike_data <- read_csv("cycling_oyster_data.csv",
                      col_types = cols(Date = col_date(format = "%Y-%m-%d")))


pound <- function(x) {
  paste0("£",format(x, big.mark = ",",
                    decimal.mark = ".",
                    trim = TRUE, scientific = FALSE))
}

"Average Monthly Travelcard Cost per Day" <- 126.80/30

oyster_card <- 126.80/30

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

total_savings <- paste0("£",sprintf("%.2f", round(travel_summary$Travelcard_total - travel_summary$current_total, 2)))



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



p1 <- ggplot(travel_summary, aes(x=variable, y=value, fill=variable, label = value)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.5)) +
  geom_text(aes(y = value + 0.1, label=paste0("£", sprintf("%.2f", round(value,2)))), position = position_dodge(0.9), vjust = -0.25, fontface = "bold") +
  scale_y_continuous(name=paste0("Total Spending from ", format(min(bike_data$Date),format="%d %B %Y"), " to ", format(max(bike_data$Date),format="%d %B %Y")), labels = pound) +
  scale_x_discrete(name="Type of Spending") +
  theme(legend.position = "bottom") + 
  ggtitle("Total and Combined Bike and Public Transit Spending") +
  annotate("text", x = 1.5, y = 1250, label=paste0("Difference: ", total_savings), fontface = "bold") +
  annotate("rect", xmin = 1.05, xmax = 1.95, ymin = 1220, ymax = 1280,
           alpha = .2, colour = "blue", fill = "blue") +
  scale_fill_discrete("")

p1

ggsave(plot = p1, filename = "p1.png",type = "cairo-png", dpi = 1000, width = 20, height = 20, units = "cm")

p2 <- ggplot(oyster_roll_gg, aes(x=Date)) +
  geom_hline(aes(yintercept=bike_average, linetype="Average Bicycle Cost per Day"), col = "#b5000e", size=1) +
  geom_hline(aes(yintercept=oyster_card,linetype="Average Monthly Travelcard Cost per Day"), col = "#01e245", size=1) +
  scale_linetype_manual(values = c(2, 2), guide = guide_legend(title = NULL, override.aes = list(color = c("#b5000e", "#01e245")))) +
  geom_line(aes(y=value, col = variable), size=1) +
  scale_color_discrete("") +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(name="Charge over previous 7 days", labels = pound) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Cycling vs Travelcard") +
  guides(col = guide_legend(ncol = 2, bycol = FALSE)) +
  geom_text(aes(label = "Bicycle Cost", x = max(Date), y = bike_average, hjust= "right", vjust = 1)) +
  geom_text(aes(label = "Monthly Zone 1-2 Travelcard", x = max(Date), y = oyster_card, hjust= "right", vjust = 1))

p2


bike_melt <- melt(bike_data, id = c("Date"))

bike_melt2 <- bike_melt %>% group_by(variable) %>% arrange(Date) %>% mutate(spending = cumsum(value))

p3 <- ggplot(bike_melt2) + geom_line(aes(x=Date,y=spending, col = variable), size=1) +
  #geom_line(aes(y=cumsum(Oyster)), size=1, col = "#01e245") +
  #geom_line(aes(y=cumsum(Bike)), size=1, col = "#b5000e") + 
  scale_y_continuous(name = "Cumulative Spending", labels = pound) + 
  scale_x_date(date_breaks = "1 month") + 
  scale_color_manual(values = c("#01e245","#b5000e"), labels = c('Oyster','Bike'), name="") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1))
p3


