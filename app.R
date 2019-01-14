
library(shiny)
library(shinyjs)
library(zoo)
library(ggplot2)
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(Cairo)
library(broom)
options(shiny.usecairo=T)

# CSS ------------------------------------------------------------
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

# UI -------------------------------------------------------------
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
  
  div(## Loading intro animation
    id = "loading-content",
    
    h2("Loading...")
  ),
  
  hidden(
    div(
      id = "app-content",
      
      column(2),
      
      # App Text ---------------------------------------------------------------
      column(8,
             fluidRow(
               p("*Updated every few days."),
               p("In a", tags$a(href="https://evanodell.com/blog/2017/02/06/cycling-vs-oyster/", "blog post"), "in February 2017 I analysed how much money I was saving by cycling to work instead of using a monthly Oyster transport pass. When I wrote that blog I had spent almost £20 more on my bike and pay-as-you-go transport pass than I would have if I bought a monthly transport pass. On 29 April 2017, less than 3 months after that blog, and despite needing a new rear wheel, I broke even, and I've been updating my data every few days, and have now built a", tags$a(href="https://shiny.rstudio.com/", "Shiny"), "app to monitor my spending on my bike and on public transport."),
               p("Of course, I had to screw this up somehow, so I bought a new bike in October 2017. My new bike was stolen in November 2018, but thanks to my insurance the cost of a replacement was not too high, although my premiums have gone up slightly and I've missed out on a no-claims discount."),
               p("I include all spending directly on my bike: the bike itself, accessories, spare parts, insurance, my storage locker, tools and maintenance. I also include non-bike costs that are the result of cycling, primarily clothing. For instance, I have bought a couple pairs of commuter trousers for cycling and include that spending in my calculations, less £40 to represent the price of a standard pair of men's trousers, on the basis that I would have had to buy new trousers anyways."),
               p("You can see in the second time series plot that since writing the blog post in February 2017 my Oyster spending has dropped off somewhat. Since analysing how much I was cycling, and how much I was spending on transport, I've become much more dedicated to riding places, no longer taking the bus or the tube if I'm feeling a little bit lazy."),
               
# selector and fine check --------------------------------------------------------
uiOutput("selector"),
em(h4(textOutput("last_update"))),
em(h4(textOutput("savings"))),
               
# UI-p1 ----------------------------------------------------------------------
h4("Total Spending and Combined Spending:"),
div(id = "plot-container", # spinner gifs
    tags$img(src = "spinner.gif", id = "loading-spinner"), plotOutput("p1")),
em(textOutput("p1_text"))),
             
# UI-p2 ----------------------------------------------------------------------
fluidRow(h4("Time Series of Spending on Pay-As-You-Go Oyster and Cycling:"),
  div(id = "plot-container",
      tags$img(src = "spinner.gif", id = "loading-spinner"), plotOutput("p2")),
  em(textOutput("p2_text"))),
             
# UI-p3 ----------------------------------------------------------------------
fluidRow(h4("Cumulative Spending in Each Category:"),
         div(id = "plot-container", tags$img(src = "spinner.gif", 
                                             id = "loading-spinner"),
             plotOutput("p3")),
         em(textOutput("p3_text"))),
# UI-p4 ----------------------------------------------------------------------
h4("Rolling average cost per day:"),
div(id = "plot-container",
    tags$img(src = "spinner.gif",
             id = "loading-spinner"),
    plotOutput("p4")),
# UI-p5 ----------------------------------------------------------------------
h4("Savings/losses over time:"),
div(id = "plot-container",
    tags$img(src = "spinner.gif", id = "loading-spinner"), plotOutput("p5")),
fluidRow(
  p(textOutput("prediction")),
  p("There are a number of obstacles to an exact cost comparison of cycling and public transport. For example, if I didn't cycle and was to go on holiday for a couple weeks, I would try to time my travelcard renewal so I'm not paying anything while not in London. There are also the intangible benefits of exercise and faster commutes from cycling, compared to the convenience and low effort required of public transport, and any purely financial comparison misses those factors.")
             )),
      column(2))))

# data-prep --------------------------------------------------------------------
bike_data_full <- read_csv("cycling_oyster_data.csv",
                           col_types = cols(
                             date = col_date(format = "%Y-%m-%d"))
)

bike_data_full$week_oyster_per_day <- case_when(
  bike_data_full$date <= "2017-01-01" ~ 32.4/7,
  bike_data_full$date <= "2018-01-01" ~ 33/7,
  bike_data_full$date <= "2019-01-01" ~ 34.1/7,
  bike_data_full$date <= "2020-01-01" ~ 35.10/7
)

bike_data_full$mon_oyster_per_day <- case_when(
  bike_data_full$date <= "2017-01-01" ~ 124.50/30,
  bike_data_full$date <= "2018-01-01" ~ 126.80/30,
  bike_data_full$date <= "2019-01-01" ~ 131.00/30,
  bike_data_full$date <= "2020-01-01" ~ 134.80/30
)

bike_data_full$annual_oyster_per_day <- case_when(
  bike_data_full$date <= "2017-01-01" ~ 1296/366,
  bike_data_full$date <= "2018-01-01" ~ 1320/365,
  bike_data_full$date <= "2019-01-01" ~ 1364/365,
  bike_data_full$date <= "2020-01-01" ~ 1404/365
)

bike_data_full$locker_cost <- case_when(
  bike_data_full$date <= "2018-01-22" ~ 60/365,
  bike_data_full$date <= "2019-01-22" ~ 30/365,
  bike_data_full$date <= "2020-01-22" ~ 40/365
)

bike_data_full$insurance <- case_when(
  bike_data_full$date >= "2018-12-17" ~ 112/365,
  bike_data_full$date >= "2018-06-18" ~ 101.80/365,
  TRUE ~ 0
)

bike_data_full$bike <- bike_data_full$bike + bike_data_full$locker_cost + 
  bike_data_full$insurance 

bike_data_full <- bike_data_full %>%
  gather(key=travelcard_type, value = travelcard_day,
         -date, -bike, -oyster, -locker_cost, -fines, -insurance)

if (Sys.getenv('SHINY_PORT') == "") {
  attr(bike_data_full, "latest") <- Sys.time()
} else {
  attr(bike_data_full, "latest") <- bike_data_full[
    bike_data_full$date == max(bike_data_full$date)
    ,][3,]
}

write_rds(bike_data_full, "bike_data_full.rds")

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
  
  bike_data_full <- read_rds("bike_data_full.rds")
  
  # Selecter ------------------------------------------------------------------
  output$selector <- renderUI({
    radioButtons("period_selection", "Select Travelcard Length",
                 choices = c("Weekly" = "week_oyster_per_day",
                             "Monthly" = "mon_oyster_per_day",
                             "Annually" = "annual_oyster_per_day"),
                 selected = "mon_oyster_per_day")
  })
  
  bike_data_subset <- reactive({
    bike_data_full[bike_data_full$travelcard_type == input$period_selection,]
  })
  
  pound <- function(x) {
    paste0("£", format(x, big.mark = ",",
                       decimal.mark = ".",
                       trim = TRUE,
                       scientific = FALSE,
                       nsmall = 0L))
  }
  
  
  df_date_time <- function(){
    attributes(bike_data_full)$latest
  }
  
  output$last_update <- renderText(
    paste0("Last Updated: ", format(max(bike_data_full$date), 
                                    format = "%e %B %Y")))
  
  output$savings <- renderText({
    
    bike_data <- bike_data_subset()
    
    current_total <- sum(bike_data$oyster, bike_data$bike)
    
    travelcard_total <- sum(bike_data$travelcard_day, bike_data$fines)
    
    totsav <- if_else(travelcard_total - current_total > 0,
                      "savings", "losses")
    
    savings <- paste0("Total ", totsav,
                      " from cycling instead of using public transport: ",
                      "£", sprintf("%.2f", abs(
                        round(travelcard_total - current_total, 2))))
    
    print(savings)
    
  })
  
# p1 ---------------------------------------------------------------------------
  output$p1 <- renderCachedPlot({
    
    bike_data <- bike_data_subset()
    
    bike_average <- mean(bike_data$bike)
    
    travel_summary <- gather(data.frame(Bike = sum(bike_data$bike),
                                        Oyster = sum(bike_data$oyster),
                                        Combined = sum(bike_data$oyster,
                                                       bike_data$bike),
                                        Hypothetical_Travelcard = sum(
                                          bike_data$travelcard_day,
                                          bike_data$fines)),
                             variable, value)
    
    travel_summary$variable <- gsub("_", " ", travel_summary$variable)
    
    travel_summary$variable <- factor(travel_summary$variable,
                                      levels=c("Bike", "Oyster", "Combined",
                                               "Hypothetical Travelcard")
    )
    
    p1 <- ggplot(travel_summary, aes(x = variable, y = value,
                                     fill = variable, label = value)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
      geom_text(aes(y = value + 0.1,
                    label = paste0("£", format(round(value, 2),
                                               big.mark = ",", nsmall = 2))),
                position = position_dodge(0.9),
                vjust = -0.25,
                fontface = "bold",
                size = 5) +
      scale_y_continuous(labels = pound, 
                         name = paste0("Total spending from\n",
                                       format(min(bike_data$date),
                                              format = "%e %B %Y"), " to ",
                                       format(max(bike_data$date),
                                              format = "%e %B %Y"))) +
      scale_x_discrete(name = "Type of Spending") +
      scale_fill_viridis_d(option = "cividis") + 
      theme(legend.position = "",
            text=element_text(size = 14),
            legend.text=element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.text.x = element_text(size = 14)) 
    
    print(p1)
    
  }, cacheKeyExpr = {list(input$period_selection, df_date_time())})
  
# p1 text ----------------------------------------------------------------------
  output$p1_text <- renderText({
    
    bike_data <- bike_data_subset()
    
    type <- case_when(
      input$selection=="week_oyster_per_day" ~ " weekly",
      input$selection=="mon_oyster_per_day" ~ " monthly",
      input$selection=="annual_oyster_per_day" ~ "n annual"
    )
    
    paste0("The red and green bars are total spending on my bike and related accessories and my pay-as-you-go Oyster spending, respectively. The brown bar is the combined total of bicycle and pay-as-you-go spending, and the yellow bar is the hypothetical total spending on a", type, " Travelcard and travel outside zone 2 covering ", format(as.character(max(bike_data$date) - min(bike_data$date)), big.mark = ","), " days, from 30 June 2016 to ", format(max(bike_data$date), format = "%e %B %Y"), ".")
    
  })  
  
# p2 --------------------------------------------------------------------------
  output$p2 <- renderCachedPlot({
    
    bike_data <- bike_data_subset()
    
    bike_average <- mean(bike_data$bike)
    
    bike_data$bike_avg <- mean(bike_data$bike)
    
    oyster_roll_gg <- broom::tidy(
      rollapply(zoo(bike_data$oyster, order.by = bike_data$date),
                7, mean, align = "right")
    ) %>% select(index, value)
    
    #oyster_roll_gg$date <- as.Date(row.names(oyster_roll_gg))
    
    names(oyster_roll_gg)[names(oyster_roll_gg)=="value"] <- "oyster_charge"
    names(oyster_roll_gg)[names(oyster_roll_gg)=="index"] <- "date"
    
    oyster_roll_gg$bike_plus_oyster <- mean(bike_data[["bike"]]) +
      oyster_roll_gg$oyster_charge
    
    oyster_roll_gg <- oyster_roll_gg %>% 
      left_join(bike_data %>%
                  select(date, travelcard_day, bike_avg))
    
    oyster_roll_gg <- gather(oyster_roll_gg, spend_type, value, -date)
    
    oyster_roll_gg <- oyster_roll_gg %>% 
      mutate(spend_type = recode(spend_type,
             "oyster_charge" = "PAYG Oyster Spending",
             "bike_plus_oyster" = "Oyster + Bike Spending",
             "bike_avg" = "Bike Average",
             "travelcard_day" = "Travelcard Average"), 
             spend_type = factor(spend_type, levels=c("Oyster + Bike Spending",
                                                      "PAYG Oyster Spending",
                                                      "Bike Average",
                                                      "Travelcard Average")))

    p2 <- ggplot(oyster_roll_gg, aes(x = date)) +
      geom_line(aes(y = value, col = spend_type, linetype = spend_type),
                size = 1) +      
      scale_colour_viridis_d("", direction = -1, end = 0.9) + 
      scale_x_date(name = "Date", date_breaks = "3 months",
                   date_labels = "%b %Y") +
      scale_y_continuous(name = "Average charge over previous 7 days",
                         labels = pound,
                         breaks = seq(0, 10, by = 2)) +
      guides(col = guide_legend(nrow = 2, bycol = TRUE)) +
      geom_text(aes(x = max(date),
                    y = bike_average,
                    hjust= 1,
                    vjust = 1.4,
                    label = paste0("Bike Average: £", format(
                      round(bike_average, 2), nsmall = 2))),
                size = 6) +
      geom_text(aes(x = max(date),
                    y = max(bike_data$travelcard_day),
                    hjust = 1,
                    vjust = -0.5,
                    label = paste0("Travelcard Average: £", format(
                      round(mean(bike_data$travelcard_day), 2),
                      nsmall = 2))),
                size = 6) +
      # geom_step(aes(y = travelcard_day,
      #               linetype = "Bicycle Cost-Per-Day"),
      #           col = "#641A80", size = 1, data = bike_data) +
      # geom_step(aes(y = bike_avg, linetype = "Travelcard Cost-Per-Day"),
      #           col = "#000004", size = 1, data = bike_data) +
      scale_linetype_manual(values = c(rep("solid", 2), rep("dashed", 2)),
                            guide = FALSE) +
      theme(legend.position = "bottom",
            legend.text=element_text(size = 14),
            text=element_text(size = 14),
            axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)
      )
    
    print(p2)
    
  }, cacheKeyExpr = {list(input$period_selection, df_date_time())})
  
# p2 text ----------------------------------------------------------------------
  output$p2_text <- renderText({
    
    bike_data <- bike_data_subset()
    
    comparison <- sum(bike_data$bike, bike_data$oyster) - 
      sum(bike_data$travelcard_day, bike_data$fines)
    
    compare <- if_else(comparison > 0, "more", "less")
    
    type <- case_when(
      input$period_selection=="week_oyster_per_day" ~ " weekly",
      input$period_selection=="mon_oyster_per_day" ~ " monthly",
      input$period_selection=="annual_oyster_per_day" ~ "n annual"
    )
    
    cost_per_day <- bike_data %>%
      mutate(year = substring(date, 1,4)) %>%
      group_by(year) %>%
      summarise(max = max(travelcard_day)) %>%
      mutate(max = sprintf("%.2f", round(max, 2)),
             sep = case_when(year==max(year) ~ " and",
                             year == min(year) ~ "",
                             TRUE ~ ","))
    
    paste0("The dark purple dashed horizontal line represents the cost-per-day of a", type, " zone 1-2 Travelcard in London over this time period: ", paste(lapply(cost_per_day, function(x) paste0(cost_per_day$sep, " £", cost_per_day$max, " in ", cost_per_day$year))[[1]], collapse = ""), ", averaging to £", sprintf("%.2f", round(mean(bike_data$travelcard_day), 2)), ". The blue dashed horizontal line represents the average daily cost of my bicycle and accessories (£", sprintf("%.2f", round(sum(bike_data$bike)/nrow(bike_data), 2)), "). The dark green line is a rolling weekly average of daily pay-as-you-go Oyster spending, and the light green line is pay-as-you-go Oyster spending combined with average daily bike costs. The average cost-per-day of my pay-as-you-go Oyster card is £", sprintf("%.2f", round((sum(bike_data$oyster)/nrow(bike_data)), 2)), ", which combined with bike spending means I have spent an average of £", sprintf("%.2f", abs(round(comparison/nrow(bike_data), 2))), " per day ", compare, " than I would using a", type, " travelcard (totals may not add up exactly due to rounding). Fixed bicycle costs are insurance (£",sprintf("%.2f", round(bike_data$insurance[bike_data$date == max(bike_data$date)]*365, 2)), " per year or £",sprintf("%.2f", round(bike_data$insurance[bike_data$date == max(bike_data$date)], 2)), " per day) and my bike locker (£",sprintf("%.2f", round(bike_data$locker_cost[bike_data$date == max(bike_data$date)]*365, 2)), " per year or £", sprintf("%.2f", round(bike_data$locker_cost[bike_data$date == max(bike_data$date)], 2)), " per day), so every day I cycle I save £", sprintf("%.2f", round(max(bike_data$travelcard_day) - (bike_data$locker_cost[bike_data$date == max(bike_data$date)] + bike_data$insurance[bike_data$date == max(bike_data$date)]), 2)), " compared to a travelcard.")

    
  })
  
# p3 ---------------------------------------------------------------------------
  
  output$p3 <- renderCachedPlot({
    
    #bike_data <- bike_data_subset()
    
    bike_melt <- bike_data_full %>%
      select(date:bike) %>%
      gather(spend_type, value, -date) %>% distinct() %>%
      group_by(spend_type) %>%
      arrange(date) %>%
      mutate(spending = cumsum(value))
    
    
    p3 <- ggplot(bike_melt) +
      geom_line(aes(x = date, y = spending, col = spend_type), size = 1) +
      scale_y_continuous(name = "Cumulative Spending", 
                         breaks = seq(0, 5000, by = 500), 
                         labels = pound) +
      scale_x_date(name = "Date", date_breaks = "3 months",
                   date_labels = "%b %Y") +
      scale_color_viridis_d(end = 0.6, 
                            labels = c("Bike Spending",
                                       "Pay-as-you-go Oyster Spending"),
                            name = "") +
      theme(legend.position = "bottom",
            legend.text=element_text(size = 14),
            text=element_text(size = 14),
            axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)
      )
    
    print(p3)
    
  }, cacheKeyExpr = {list(df_date_time())})
  
# p3 text ----------------------------------------------------------------------
  output$p3_text <- renderText({
    
    bike_data <- bike_data_subset()
    
    paste0("Cumulative spending in each category over ", 
           as.character(max(bike_data$date) - min(bike_data$date)),
           " days, from 30 June 2016 to ", format(max(bike_data$date),
                                                  format = "%e %B %Y"), ".")
    
  })
  
# p4 ---------------------------------------------------------------------------
  output$p4 <- renderCachedPlot({
    
    bike_data <- bike_data_full %>%
      select(date:bike) %>% distinct()
    
    bike_data$bike_cumsum <- (
      cumsum(bike_data$bike)/as.numeric(bike_data$date - as.Date("2016-06-29"))
    )
    
    bike_data$oyster_cumsum <- (
      cumsum(bike_data$oyster)/as.numeric(bike_data$date - as.Date("2016-06-29"))
    )
    
    bike_roll_gg <-
      inner_join(tidy(rollapply(zoo(bike_data$bike_cumsum,
                                        order.by = bike_data$date), 7, mean)) %>%
              mutate(series = NULL) %>% rename(bike = value) ,
            tidy(rollapply(zoo(bike_data$oyster_cumsum,
                                        order.by = bike_data$date), 7, mean)) %>%
              mutate(series = NULL) %>% rename(oyster = value) ) %>%
      rename(date = index)
    
    
    #bike_roll_gg$date <- as.Date(row.names(bike_roll_gg))
    
    bike_roll_gg <- bike_roll_gg %>% 
      gather(type, spending, -date)
    
    
    # %>%
    #   filter(date >= as.Date("2016-07-14"))
    p4 <- ggplot(bike_roll_gg) +
      geom_line(aes(x = date, y = spending, group = type, col = type), size = 1) +
      scale_y_continuous(name = "7 Day rolling average cost per day",
                         labels = pound, limits = c(NA, 10),
                         breaks = c(0, 2, 4, 6, 8, 10), trans = "log10") +
      scale_x_date(name = "Date", date_breaks = "3 months",
                   date_labels = "%b %Y",
                   limits = c(as.Date("2016-06-30"), NA)) +
      scale_color_viridis_d( end = 0.6,
                             labels = c("Bike Spending", "Oyster Spending")) +
      theme(legend.position = "bottom",
            legend.text=element_text(size = 14),
            text=element_text(size = 14),
            axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      labs(col = "")
    
    print(p4)
    
  }, cacheKeyExpr = {list(df_date_time())})
  
# p5 ---------------------------------------------------------------------------
  output$p5 <- renderCachedPlot({
    
    bike_data <- bike_data_subset()
    
    bike_data$gain_loss <- cumsum(bike_data$travelcard_day) -
      (cumsum(bike_data$bike) + cumsum(bike_data$oyster)) +
      cumsum(bike_data$fines)
    
    max_savings <- if_else(max(bike_data$gain_loss) >= 0,
                           "Max savings: £",
                           "Min loss: £")
    
    p5 <- ggplot(bike_data) +
      geom_hline(yintercept = 0, colour = "red", size = 0.5, alpha = 0.7) +
      geom_hline(yintercept = max(bike_data$gain_loss), colour = "seagreen3",
                 size = 0.5, alpha = 0.7) +
      geom_line(aes(x = date, y = gain_loss), size = 1, 
                colour = "#932667", alpha = 0.9) +
      geom_text(aes(x = bike_data$date[
        bike_data$gain_loss == max(bike_data$gain_loss)
        ],
        y = max(bike_data$gain_loss),
        hjust = 0.8,
        vjust = 0,
        label = paste0(max_savings, 
                       sprintf("%.2f", round(max(bike_data$gain_loss), 2))
        )), size = 6) +
      geom_text(aes(x = as.Date("2017-10-20"),
                    y = bike_data$gain_loss[
                      bike_data$date == bike_data$date[
                        bike_data$gain_loss == min(bike_data$gain_loss)
                        ] - 15
                      ],
                    hjust= 0,
                    vjust = 0.5,
                    label = "Bought new bike"), size = 6) +
      geom_text(aes(x = bike_data$date[
        bike_data$gain_loss == min(bike_data$gain_loss)
        ],
        y = min(bike_data$gain_loss),
        hjust= 1.06,
        vjust = 0.5,
        label = paste0("Max loss: £",
                       sprintf("%.2f", round(min(bike_data$gain_loss), 2)))),
        size = 6) +
      geom_text(aes(x = as.Date("2018-02-02"),
                    y = bike_data$gain_loss[
                      bike_data$date == as.Date("2018-02-02")
                      ],
                    hjust= -0.03,
                    vjust = 0.6,
                    label = "Sold old bike"), size = 6) +
      geom_text(aes(x = as.Date("2018-11-28"),
                    y = bike_data$gain_loss[
                      bike_data$date == as.Date("2018-11-28")
                      ],
                    hjust= 0.4,
                    vjust = -1.9,
                    label = "Bike Stolen"), size = 6) +
      scale_y_continuous(name = "Savings/Losses over Time",
                         labels = pound,
                         breaks = seq(-1200, 1000, by = 100)) +
      scale_x_date(name = "Date", date_breaks = "3 months",
                   date_labels = "%b %Y") +
      scale_color_manual(values = c("#932667"),
                         labels = c("Bike Spending")) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 14),
            text = element_text(size = 14),
            axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14))
    
    print(p5)
    
  }, cacheKeyExpr = {list(input$period_selection, df_date_time())})
  
}
# Caching
shinyOptions(cache = diskCache("./cycling-oyster-cache"))

# Run the application
shinyApp(ui = ui, server = server)
