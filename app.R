
library(shiny)
library(shinyjs)
library(ggplot2)
library(readr)
library(dplyr)
library(zoo)
library(broom)
library(tidyr)
library(Cairo)
library(ggrepel)
options(shiny.usecairo=T)
theme_set(theme_bw())

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
               p("I include all spending directly on my bike: the bike itself, accessories, spare parts, insurance, my storage locker, tools and maintenance. I also include non-bike costs that are the result of cycling, primarily clothing, but also my", tags$a(href = "https://membership.lcc.org.uk/join", "London Cycling Campaign membership"), ". For instance, I have bought a couple pairs of commuter trousers for cycling and include that spending in my calculations, less £40 to represent the price of a standard pair of men's trousers, on the basis that I would have had to buy new trousers anyways."),
               p("You can see in the second time series plot that since writing the blog post in February 2017 my Oyster spending has dropped off somewhat. Since analysing how much I was cycling, and how much I was spending on transport, I've become much more dedicated to riding places, no longer taking the bus or tube if I'm feeling a little bit lazy."),
               
# selector and fine check --------------------------------------------------------
uiOutput("selector"),
#em(h4(textOutput("last_update"))),
em(h4(htmlOutput("last_update2"))),
               
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
  p("There are a number of obstacles to an exact cost comparison of cycling and public transport. For example, if I didn't cycle and was to go on holiday for a couple weeks, I would try to time my travelcard renewal so I'm not paying anything while not in London. There are also the intangible benefits of exercise and faster commutes from cycling, compared to the convenience and low effort required of public transport, and any purely financial comparison misses those factors."),
  p("All the code and data in this app is available on ",
    tags$a(href="https://github.com/evanodell/cycling-v-oyster", "Github", "."))
             )),
      column(2))))

# data-prep --------------------------------------------------------------------
bike_data_full <- read_csv("cycling_oyster_data.csv",
                           col_types = cols(
                             date = col_date(format = "%Y-%m-%d"))
)

bike_data_full$mon_oyster_per_day <- case_when(
  bike_data_full$date <= "2017-01-01" ~ 124.50/30,
  bike_data_full$date <= "2018-01-01" ~ 126.80/30,
  bike_data_full$date <= "2019-01-01" ~ 131.00/30,
  bike_data_full$date <= "2020-01-01" ~ 134.80/30,
  bike_data_full$date <= "2021-01-01" ~ 138.70/30
)

bike_data_full$annual_oyster_per_day <- case_when(
  bike_data_full$date <= "2017-01-01" ~ 1296/366,
  bike_data_full$date <= "2018-01-01" ~ 1320/365,
  bike_data_full$date <= "2019-01-01" ~ 1364/365,
  bike_data_full$date <= "2020-01-01" ~ 1404/365,
  bike_data_full$date <= "2021-01-01" ~ 1444/366
)

bike_data_full$insurance <- case_when(
  bike_data_full$date >= "2018-12-17" ~ 112/365,
  bike_data_full$date >= "2018-06-18" ~ 101.80/365,
  TRUE ~ 0
)

bike_data_full$bike <- bike_data_full$bike + 
  bike_data_full$insurance 

bike_data_full <- bike_data_full %>%
  gather(key=travelcard_type, value = travelcard_day,
         -date, -bike, -oyster, -fines, -insurance)

bike_data_full <- bike_data_full %>%
  group_by(travelcard_type) %>%
  mutate(gain_loss = cumsum(travelcard_day) - 
           (cumsum(bike) + cumsum(oyster)) + cumsum(fines)) %>%
  ungroup()

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
    radioButtons("period_selection",
                 "Select Travelcard Length for Comparison",
                 choices = c("Annually" = "annual_oyster_per_day",
                             "Monthly" = "mon_oyster_per_day"
                             ),
                 selected = "annual_oyster_per_day", width = "100%")
  })
  
  bike_data_subset <- reactive({
    bike_data_full %>% filter(travelcard_type==input$period_selection)
  })

  df_date_time <- function(){
    attributes(bike_data_full)$latest
  }

# Update details ----------------------------------------------------------

  output$last_update2 <- renderUI({
    str1 <- paste0("Last Updated: ", format(max(bike_data_full$date),
                                            format = "%e %B %Y"))
    
    bike_data <- bike_data_subset()
    
    current_total <- sum(bike_data$oyster, bike_data$bike)
    
    travelcard_total <- sum(bike_data$travelcard_day, bike_data$fines)
    
    totsav <- if_else(travelcard_total > current_total, "savings", "losses")
    
    savings <- paste0("Current ", totsav,
                      " from cycling instead of using public transport: ",
                      "£", sprintf("%.2f", abs(
                        round(travelcard_total - current_total, 2))))
    
    totsav2 <- if_else(travelcard_total > current_total,
                       "Maximum savings", "Minimum losses")
    
    str2 <- if_else(
      bike_data$date[
        bike_data$gain_loss == max(bike_data$gain_loss)
        ] == max(bike_data$date), "",
      paste0(totsav2, " of £",
             sprintf("%.2f", abs(round(max(bike_data$gain_loss), 2))),
             " on ", format(bike_data$date[
               bike_data$gain_loss == max(bike_data$gain_loss)
               ],format = "%e %B %Y"))
      )

    HTML(paste(str1, savings, str2, sep = '<br/>'))

  })
  
# p1 ---------------------------------------------------------------------------
  output$p1 <- renderCachedPlot({
    
    bike_data <- bike_data_subset()
  
    t_sum <- bike_data %>%
      summarise(Bike = sum(bike),
                Oyster = sum(oyster),
                `Hypothetical Travelcard` = sum(travelcard_day, fines)) %>%
      mutate(Combined = sum(Bike, Oyster)) %>% 
      gather() %>%
      mutate(key = factor(key, levels=c("Bike", "Oyster", "Combined",
                                        "Hypothetical Travelcard")))
    
    p1 <- ggplot(t_sum, aes(x = key, y = value, fill = key, label = value)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.5),
               alpha = 0.8) +
      geom_text(aes(y = value + 0.1,
                    label = paste0("£", format(round(value, 2),
                                               big.mark = ",", nsmall = 2))),
                position = position_dodge(0.9),
                vjust = -0.25,
                fontface = "bold",
                size = 5) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "£"), 
                         name = paste0("Total spending from\n",
                                       format(min(bike_data$date),
                                              format = "%e %B %Y"), " to ",
                                       format(max(bike_data$date),
                                              format = "%e %B %Y"))) +
      scale_x_discrete(name = "Type of Spending") +
      scale_fill_viridis_d(option = "D") + 
      theme(legend.position = "",
            text=element_text(size = 14),
            legend.text=element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.text.x = element_text(size = 14)) 
    
    p1
    
  }, cacheKeyExpr = {list(input$period_selection, df_date_time())})
  
# p1 text ----------------------------------------------------------------------
  output$p1_text <- renderText({
    
    bike_data <- bike_data_subset()
    
    type <- case_when(
      input$selection=="mon_oyster_per_day" ~ " monthly",
      input$selection=="annual_oyster_per_day" ~ "n annual"
    )
    
    paste0("The purple and blue bars are total spending on my bike and related accessories and my pay-as-you-go Oyster spending, respectively. The green bar is the combined total of bicycle and pay-as-you-go spending, and the yellow bar is the hypothetical total spending on a", type, " Travelcard and travel outside zone 2 covering ", format(as.numeric(max(bike_data$date) - min(bike_data$date)), big.mark = ","), " days, from 30 June 2016 to ", format(max(bike_data$date), format = "%e %B %Y"), ".")
    
  })  
  
# p2 --------------------------------------------------------------------------
  output$p2 <- renderCachedPlot({
    
    bike_data <- bike_data_subset()
    
    bike_average <- mean(bike_data$bike)
    
    bike_data$bike_avg <- bike_average
    
    label_df2 <- tibble(
      label = c(paste0("Bike Average: £",
                       format(round(bike_average, 2), nsmall = 2)),
                paste0("Travelcard Average: £", format(
                  round(mean(bike_data$travelcard_day), 2),
                  nsmall = 2)),
                paste0("Travelcard Currently: £", format(
                  round(max(bike_data$travelcard_day), 2),
                  nsmall = 2))
                ),
      date = c(max(bike_data$date), mean(bike_data$date), max(bike_data$date)),
      value = c(bike_average, mean(bike_data$travelcard_day),
                max(bike_data$travelcard_day)),
      hjust = c(1, 1, 1),
      vjust = c(1.4, -0.5, -0.5)
    )
    
    bike_data2 <- bike_data %>% 
      mutate(bike_oyster = bike_avg + oyster) %>%
      pivot_longer(cols = c("travelcard_day", "bike_avg"),
                   names_to = "line_label", values_to = "line_value") %>% 
      pivot_longer(cols = c("oyster", "bike_oyster"),
                   names_to = "smooth_label", values_to = "smooth_value") %>%
      mutate(smooth_label = recode(smooth_label,
                                  "oyster" = "PAYG Oyster Spending",
                                  "bike_oyster" = "Oyster + Bike Spending"),
             line_label = recode(line_label,
                                 "bike_avg" = "Bike Average",
                                 "travelcard_day" = "Travelcard Average"))

    p2 <- ggplot(bike_data2, aes(x = date)) +
      geom_line(aes(y = line_value, col = line_label, linetype = line_label),
                size = 1.1) + 
      geom_smooth(aes(y = smooth_value, col = smooth_label,
                      linetype = smooth_label),
                  size = 1.1, method = "loess", span = 0.15, se = FALSE) + 
      scale_colour_viridis_d("", direction = -1, end = 0.85, alpha = 0.8) + 
      scale_x_date(name = "Date", 
                   breaks = seq(as.Date("2016-06-30"), 
                                as.Date(max(bike_data$date)) + 60,
                                by="3 months"),
                   date_labels = "%b %Y") +
      scale_y_continuous(name = "Smoothed average",
                         labels = scales::dollar_format(prefix = "£")) +
      guides(col = guide_legend(nrow = 2, bycol = TRUE)) +
      geom_text(aes(x = date, y = value, label = label), data = label_df2,
                      hjust = label_df2$hjust,
                      vjust = label_df2$vjust,
                      size = 6)  +
      scale_linetype_manual(values = c("dashed", "solid", "solid", "dashed"),
                            guide = FALSE) +
      theme(legend.position = "bottom",
            legend.text=element_text(size = 14),
            text=element_text(size = 14),
            axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)
            )
    
    p2
    
  }, cacheKeyExpr = {list(input$period_selection, df_date_time())})
  
# p2 text ----------------------------------------------------------------------
  output$p2_text <- renderText({
    
    bike_data <- bike_data_subset()
    
    comparison <- sum(bike_data$bike, bike_data$oyster) - 
      sum(bike_data$travelcard_day, bike_data$fines)
    
    compare <- if_else(comparison > 0, "more", "less")
    
    type <- case_when(
      input$period_selection == "mon_oyster_per_day" ~ " monthly",
      input$period_selection == "annual_oyster_per_day" ~ "n annual"
    )
    
    cost_per_day <- bike_data %>%
      mutate(year = substring(date, 1,4)) %>%
      group_by(year) %>%
      summarise(max = max(travelcard_day)) %>%
      mutate(max = sprintf("%.2f", round(max, 2)),
             sep = case_when(year==max(year) ~ " and",
                             year == min(year) ~ "",
                             TRUE ~ ","))
    
    last_month_cpd <- bike_data %>%
      top_n(30, date) %>% 
      summarise(total = mean(oyster),
                bike = sum(bike),
                oyster_total = sum(oyster) - sum(fines),
                travelcard = sum(travelcard_day)) %>% 
      mutate(save_loss = (travelcard - (bike + oyster_total)),
             save_loss_text = if_else(save_loss > 0, "saved", "lost"))
    
    paste0("The dark purple dashed horizontal line represents the cost-per-day of a", type, " zone 1-2 Travelcard in London over this time period: ", paste(lapply(cost_per_day, function(x) paste0(cost_per_day$sep, " £", cost_per_day$max, " in ", cost_per_day$year))[[1]], collapse = ""), ", averaging to £", sprintf("%.2f", round(mean(bike_data$travelcard_day), 2)), ". The green dashed horizontal line represents the average daily cost of my bicycle and accessories (£", sprintf("%.2f", round(sum(bike_data$bike)/nrow(bike_data), 2)), "). The blue line is a rolling average of pay-as-you-go Oyster spending over the previous month, and the light green line is pay-as-you-go Oyster spending combined with average daily bike costs. The average cost-per-day of my pay-as-you-go Oyster card is £", sprintf("%.2f", round((sum(bike_data$oyster)/nrow(bike_data)), 2)), ", which combined with bike spending means I have spent an average of £", sprintf("%.2f", abs(round(comparison/nrow(bike_data), 2))), " per day ", compare, " than I would using a", type, " travelcard (totals may not add up exactly due to rounding). Every day I cycle I save £", sprintf("%.2f", round(max(bike_data$travelcard_day) - (bike_data$insurance[bike_data$date == max(bike_data$date)]), 2)), " compared to a", type, " travelcard.")

# "Over the past month, my Oyster travel has cost an average of £", sprintf("%.2f", round(last_month_cpd$total, 2)), " per day, and I have ", last_month_cpd$save_loss_text, " £", sprintf("%.2f", round(last_month_cpd$save_loss, 2)), " compared to a", type, " travelcard."
    
      })
  
# p3 ---------------------------------------------------------------------------
  
  output$p3 <- renderCachedPlot({
    
    bike_melt <- bike_data_full %>%
      select(date:bike) %>%
      gather(spend_type, value, -date) %>%
      distinct() %>%
      group_by(spend_type) %>%
      arrange(date) %>%
      mutate(spending = cumsum(value))
    
    p3 <- ggplot(bike_melt) +
      geom_line(aes(x = date, y = spending, col = spend_type),
                size = 1, alpha = 0.8) +
      scale_y_continuous(name = "Cumulative Spending", 
                         breaks = seq(0, 5000, by = 500), 
                         labels = scales::dollar_format(prefix = "£")) +
      scale_x_date(name = "Date", 
                   breaks = seq(as.Date("2016-06-30"), 
                                as.Date(max(bike_data_full$date)) + 60,
                                by="3 months"),
                   date_labels = "%b %Y") +
      scale_color_viridis_d(end = 0.6, 
                            labels = c("Bike Spending",
                                       "Pay-as-you-go Oyster Spending"),
                            name = "") +
      theme(legend.position = "bottom",
            legend.text=element_text(size = 14),
            text=element_text(size = 14),
            axis.text.x = element_text(angle = 30, hjust = 1, size = 14)
      )
    
    p3
    
  }, cacheKeyExpr = {list(df_date_time())})
  
# p3 text ----------------------------------------------------------------------
  output$p3_text <- renderText({
    
    bike_data <- bike_data_subset()
    
    paste0("Cumulative spending in on my bike and oyster card over ", 
           format((max(bike_data$date) - min(bike_data$date)), big.mark = ","),
           ", from 30 June 2016 to ", format(max(bike_data$date),
                                                  format = "%e %B %Y"), ".")
    
  })
  
# p4 ---------------------------------------------------------------------------
  output$p4 <- renderCachedPlot({
    
    bike_data <- bike_data_full %>%
      select(date:bike) %>%
      distinct() 
    
    bike_data$bike_cumsum <- (
      cumsum(bike_data$bike)/as.numeric(bike_data$date - as.Date("2016-06-29"))
    )
    
    bike_data$oyster_cumsum <- (
      cumsum(bike_data$oyster)/as.numeric(bike_data$date - as.Date("2016-06-29"))
    )
    
    bike_roll <- inner_join(
      tidy(
        rollapplyr(zoo(bike_data$bike_cumsum, order.by = bike_data$date),
                   30, mean)
        ) %>%
        mutate(series = NULL) %>%
        rename(bike = value), 
      tidy(
          rollapplyr(zoo(bike_data$oyster_cumsum, order.by = bike_data$date),
                      30, mean)
          ) %>%
        mutate(series = NULL) %>%
        rename(oyster = value)
      ) %>%
      rename(date = index) %>%
      gather(type, spending, -date)
  
    p4 <- ggplot(bike_roll) +
      geom_line(aes(x = date, y = spending, group = type, col = type),
                size = 1.05) +
      coord_cartesian(ylim=c(0, 5)) + 
      scale_y_continuous(name = "30 day rolling average",
                         labels = scales::dollar_format(prefix = "£")) +
      scale_x_date(name = "Date", 
                   breaks = seq(as.Date("2016-06-30"), 
                                as.Date(max(bike_data$date)) + 60,
                                by="3 months"),
                   date_labels = "%b %Y") +
      scale_color_viridis_d(end = 0.6,
                            labels = c("Bike Spending", "Oyster Spending"),
                            alpha = 0.8) +
      theme(legend.position = "bottom",
            legend.text=element_text(size = 14),
            text=element_text(size = 14),
            axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      labs(col = "") 
    
    p4
    
  }, cacheKeyExpr = {list(df_date_time())})
  
# p5 ---------------------------------------------------------------------------
  output$p5 <- renderCachedPlot({
    
    bike_data <- bike_data_subset()
    
    label_df5 <- tibble(
      label = c(paste0(if_else(max(bike_data$gain_loss) >= 0,
                               "Max savings: £",
                               "Min loss: £") ,
                       sprintf("%.2f", max(bike_data$gain_loss))),
                paste0("Max loss: £",
                       sprintf("%.2f", min(bike_data$gain_loss))),
                "Sold old bike", "Bike stolen"),
      date = c(bike_data$date[bike_data$gain_loss == max(bike_data$gain_loss)],
               bike_data$date[bike_data$gain_loss == min(bike_data$gain_loss)],
               as.Date("2018-02-02"), as.Date("2018-11-28")),
      value = c(max(bike_data$gain_loss), min(bike_data$gain_loss),
                bike_data$gain_loss[bike_data$date == as.Date("2018-02-02")],
                bike_data$gain_loss[bike_data$date == as.Date("2018-11-28")]),
      nudge_y = c(50, -50, -50, 120),
      nudge_x = c(-350, 300, 150, 175)
    )
    
    p5 <- ggplot(bike_data) +
      geom_hline(yintercept = 0, colour = "red", size = 0.5, alpha = 0.7) +
      geom_hline(yintercept = max(bike_data$gain_loss), colour = "seagreen3",
                 size = 0.5, alpha = 0.7) +
      geom_line(aes(x = date, y = gain_loss), size = 1, 
                colour = "#932667", alpha = 0.8) + 
      geom_text_repel(aes(x = date, y = value,
                          label = label), data = label_df5,
                      nudge_y = label_df5$nudge_y,
                      nudge_x = label_df5$nudge_x,
                      size = 6, force = 10, direction = "both", 
                      arrow = arrow(length = unit(0.03, "npc")),
                      point.padding = 0.5)  +
      scale_y_continuous(name = "Savings/Losses over Time",
                         labels = scales::dollar_format(prefix = "£"),
                         breaks = seq(-1200, 1000, by = 200),
                         expand = expansion(mult = c(0.05, 0),
                                            add = c(0, 150))) +
      scale_x_date(name = "Date", 
                   breaks = seq(as.Date("2016-06-30"), 
                                as.Date(max(bike_data$date)) + 60,
                                by="3 months"),
                   date_labels = "%b %Y") +
      scale_color_manual(values = c("#932667"),
                         labels = c("Bike Spending")) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 14),
            text = element_text(size = 14),
            axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14))
    
    p5
    
  }, cacheKeyExpr = {list(input$period_selection, df_date_time())})
  
}
# Caching
shinyOptions(cache = diskCache("./cycling-oyster-cache"))

# Run the application
shinyApp(ui = ui, server = server)
