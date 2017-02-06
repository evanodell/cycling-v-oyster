

library(readr)
bike_data <- read_csv("Cycling and Oyster.csv",
                                    col_types = cols(Date = col_date(format = "%d/%m/%Y")))

library(zoo)
library(ggplot2)
library(data.table)
library(scales)
library(grid)

summary(bike_data)

"Average Monthly Travelcard Cost per Day" <- 126.80/30

oyster_card <- 126.80/30

"Average Bicycle Cost per Day" <- mean(bike_data[["Bike"]])

bike_average <- mean(bike_data[["Bike"]])

travel_summary <- data.frame(bike_total = round(sum(bike_data[["Bike"]]),2),
                         oyster_total = round(sum(bike_data[["Oyster"]]),2),
                         current_total = round(sum(bike_data[["Oyster"]]) + sum(bike_data[["Bike"]]),2),
                         travelcard_total = round(nrow(bike_data) * oyster_card,2)
)

total_savings <- sprintf("%.2f", round(travel_summary$travelcard_total - travel_summary$current_total, 2))

travel_summary$class <- NA

travel_summary <- melt(travel_summary, id = "class")

travel_summary$class <- NULL

## Recoding travel_summary$variable into travel_summary$variable
## Reordering travel_summary$variable

travel_summary$variable <- as.character(travel_summary$variable)
travel_summary$variable[travel_summary$variable == "bike_total"] <- "Bike Total"
travel_summary$variable[travel_summary$variable == "oyster_total"] <- "Oyster Total"
travel_summary$variable[travel_summary$variable == "current_total"] <- "Current Total"
travel_summary$variable[travel_summary$variable == "travelcard_total"] <- "Travelcard Total"
travel_summary$variable <- factor(travel_summary$variable, levels=c("Bike Total", "Oyster Total", "Current Total", "Travelcard Total"))

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


pound <- function(x) {
  paste0("£",format(x, big.mark = " ",
                    decimal.mark = ",",
                    trim = TRUE, scientific = FALSE))
  }

p1 <- ggplot(oyster_roll_gg, aes(x=Date)) +
  geom_hline(aes(yintercept=bike_average, linetype="Average Bicycle Cost per Day"), colour = "#b5000e", size=1) +
  geom_hline(aes(yintercept=oyster_card,linetype="Average Monthly Travelcard Cost per Day"), color = "#01e245", size=1) +
  scale_linetype_manual(values = c(2, 2),
                        guide = guide_legend(title = NULL, override.aes = list(color = c("#b5000e", "#01e245")))) +
  geom_line(aes(y=value, col = variable), size=1) +
  scale_color_discrete("") +
  #geom_text(aes(x=min(Date), y=bike_average, label = "Bicycle Cost", hjust = "left", vjust = -0.5)) +
  #geom_text(aes(x=min(Date), y=oyster_card, label = "Monthly Zone 1-2 Travel Card", hjust = "left", vjust = -0.5)) +
  scale_x_date(date_breaks = "1 week") +
  scale_y_continuous(name="Charge over previous 7 days", labels = pound) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Cycling vs Travelcard") +
  guides(col = guide_legend(ncol=2,nrow = 2, byrow = TRUE)) +
  geom_text(aes(label = "Bicycle Cost", x = max(Date), y = bike_average, hjust= "right", vjust = 1)) +
  geom_text(aes(label = "Monthly Zone 1-2 Travelcard", x = max(Date), y = oyster_card, hjust= "right", vjust = 1))

p1


p2 <- ggplot(travel_summary, aes(x=variable, y=value, fill=variable, label = value)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.5)) +
  geom_text(aes(y = value + 0.1, label=paste0("£", value)), position = position_dodge(0.9), vjust = -0.25, fontface = "bold") +
  scale_y_continuous(name="Total Spending from 2016-06-30 to 2017-02-03", labels = pound) +
  scale_x_discrete(name="Type of Spending") +
  ggtitle("Total Cost of Different Transport Modes") +
  theme(legend.position = "bottom") +
  annotate("text", x = 1.5, y = 750, label=paste0("Difference: £", total_savings), fontface = "bold") +
  annotate("rect", xmin = 1.15, xmax = 1.85, ymin = 720, ymax = 780,
               alpha = .2, colour = "blue", fill = "blue") +
  scale_fill_discrete("")

p2


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1,p2,cols=1)

require(ggplot2)
require(gridExtra)
library(ggthemes)
