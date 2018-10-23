setwd("C:/Users/Canadice/Downloads/saves")
require(dplyr)
require(stringr)
require(ggplot2)
require(lubridate)

test <- lapply(X = list.files(), FUN = function(x) {
  data.frame(read.csv(file = x), x)
  })

data <- test %>% 
  Reduce(function(dtf1,dtf2) suppressWarnings(bind_rows(dtf1,dtf2)), .)

data$x <- ymd(str_extract(data$x, pattern = "[0-9]{4}\\.[0-9]{1,2}\\.[0-9]"))

data$tag[data$tag %in% c("CAS", "ARA", "SPA")] <- "SPA"
data$tag[data$tag %in% c("BRA", "PRU")] <- "PRU"
data$tag[data$tag %in% c("ENG", "SCO", "GBR", "LEI")] <- "GBR"
data$tag[data$tag %in% c("MOS", "RUS")] <- "RUS"
data$tag[data$tag %in% c("POL", "LIT", "MAZ", "MOL", "PLC")] <- "PLC"
data$tag[data$tag %in% c("KOL", "WES")] <- "WES"
data$tag[data$tag %in% c("HOL", "NED")] <- "NED"

hre <- c("PAL", "PAP", "BRA", "PRU", "HAB", "HOL", "NED", "HSA", "COL", "WES", "GEN", "SWI", "VEN", "BOH")
gp <- c("CAS", "SPA", "POR", "FRA", "ENG", "GBR", "TUR", "POL", "PLC", "MUS", "RUS")
neut <- c("HUN", "MAM", "SWE")
players <- c(hre, gp, neut)

data$group<- NA
data$group[which(data$tag %in% hre)] <- "HRE"
data$group[which(data$tag %in% gp)] <- "GP"
data$group[which(data$tag %in% neut)] <- "neutral"

data$hre <- data$tag
data$hre[which(data$tag %in% hre)] <- "HRE"

data$players <- FALSE
data$players[which(data$tag %in% players)] <- TRUE

# Groupings

vis_time <- function(variable) {

  agg_data <- group_by(.data = data, group, x) %>%
    summarize(n = sum(get(variable), na.rm = TRUE))
  
  ggplot(data = agg_data[!is.na(agg_data$group),]) + aes(x = x, y = n, color = group, group = group) + geom_line(size = 1.1) + theme_bw() +
    labs(y = paste("Sum of \n ", variable), x = "Time")
}

vis_time("development")
vis_time("mil_strength")

# Players
agg_data <- group_by(.data = data[data$players,], hre, x) %>%
  summarize(n = sum(mil_strength, na.rm = TRUE))

cols1 <- c("FRA" = "royalblue", "GBR" = "red", "HUN" = "plum4", "MAM" = "sandybrown",
          "PLC" = "maroon2", "POR" = "seagreen1", "RUS" = "springgreen4", "SPA" = "yellow3",
          "SWE" = "blue", "TUR" = "olivedrab1")

p1 <- ggplot(data = agg_data[agg_data$hre != "HRE",]) + aes(x = x, y = n, color = hre, group = hre) + geom_line(size = 1.1) + theme_bw() +
        labs(y = paste("Sum of \n mil_strength"), x = "Time", legend = "Country") +
        scale_color_manual("Country", values = cols1) +
        scale_y_continuous(limits = c(0, 1200))

cols2 <- c("BOH" = "tan3", "GEN" = "lightgoldenrod2", "HAB" = "tomato2", "HSA" = "lightblue2",
          "NED" = "orange1", "PAL" = "mediumaquamarine", "PAP" = "black", 
          "PRU" = "seashell4", "SWI" = "saddlebrown", "VEN" = "lightseagreen", "WES" = "mediumpurple3")

p2 <- ggplot(data = data[data$hre == "HRE",]) + aes(x = x, y = mil_strength, color = tag, group = tag) + geom_line(size = 1.1) + theme_bw() +
        labs(y = paste("Sum of \n mil_strength"), x = "Time", legend = "Country") +
        scale_color_manual("Country", values = cols2) +
        scale_y_continuous(limits = c(0, 1200))

gridExtra::grid.arrange(p1, p2)

# Players
agg_data <- group_by(.data = data[data$players,], hre, x) %>%
  summarize(n = sum(est_month_income, na.rm = TRUE))

p1 <- ggplot(data = agg_data[agg_data$hre != "HRE",]) + aes(x = x, y = n, color = hre, group = hre) + geom_line(size = 1.1) + theme_bw() +
  labs(y = paste("Sum of \n est_month_income"), x = "Time", legend = "Country") +
  scale_color_manual("Country", values = cols1) 

p2 <- ggplot(data = data[data$hre == "HRE",]) + aes(x = x, y = est_month_income, color = tag, group = tag) + geom_line(size = 1.1) + theme_bw() +
  labs(y = paste("Sum of \n est_month_income"), x = "Time", legend = "Country") +
  scale_color_manual("Country", values = cols2) 

gridExtra::grid.arrange(p1, p2)
