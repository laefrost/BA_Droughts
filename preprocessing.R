library(dplyr)
library(ggplot2)
library(mgcv)
library(reshape2)
library(lubridate)
library(nnet)
library(deepregression)
library(tidyr)
library(longCatEDA)
library(lattice)

# ------------------------------ Load and convert data into time series format (df)
df_og <- read.csv2("GWL_1900-2010.csv", sep = ";")
rel_gwl <- c("BM", "HFA", "HNA", "HNFA", "NEA", "SEA")
df <- df_og

# change format from long  to wide
df <- melt(df, id = "JAHRMONAT")
# create date column
df$day <- as.numeric(gsub("\\D", "", df$variable))
df$day <-
  ifelse(df$day >= 10, as.character(df$day), paste(0, df$day, sep = ""))
df$date <- ymd(paste(df$JAHRMONAT, df$day, sep = ""))

# create additional covariates
df <- df %>% arrange(date) %>% drop_na(date) %>%
  mutate(
    month = as.numeric(format(date, "%m")),
    year = as.numeric(format(date, "%Y")),
    value_og = value,
    value = as.factor(ifelse(value %in% rel_gwl, value, "other")),
    season = as.factor(
      case_when(
        month %in% c("3", "4", "5") ~ "spring",
        month %in% c("6", "7", "8") ~ "summer",
        month %in% c("9", "10", "11") ~ "fall",
        TRUE ~ "winter"
      )
    ),
    season_num = as.numeric(season),
    lag_1 = lag(value, n = 1),
    lag_2 = lag(value, n = 2),
    lag_3 = lag(value, n = 3),
    lag_4 = lag(value, n = 4),
    lag_5 = lag(value, n = 5),
    lag_6 = lag(value, n = 6),
    lag_7 = lag(value, n = 7),
    lag_8 = lag(value, n = 8),
    lag_9 = lag(value, n = 9),
    lag_10 = lag(value, n = 10),
    date_numeric = as.numeric(date),
    day_year = lubridate::yday(date),
    value_num = as.numeric(as.factor(value)) - 1,
    lag_1_num = as.factor(as.numeric(as.factor(lag_1)) - 1),
    lag_2_num = as.factor(as.numeric(as.factor(lag_2)) - 1),
    lag_3_num = as.factor(as.numeric(as.factor(lag_3)) - 1)
  )

# remove missing day
df <- df[df$date != "1942-11-30", ]

# relevel gwl to "Other" as reference category
df$value <- relevel(df$value, ref = "other")

# remove NA values for lags
df <- df[-(1:10), , drop = FALSE]

str(df)


# ------------------------------ Load and rescale images
imgs <- readRDS("mslp_z500.rds")
imgs_train <- imgs
imgs_train <- array_reshape(imgs_train, c(40541, 39, 16, 2))
imgs_train <- imgs_train/255
# mslp: mean sea level pressure
# z500: Geopotential
# Format: 40541 x 39 x 16 x 2 --> 2 wg. mslp und z500
str(imgs_train)
imgs_train <- imgs_train[11:40541, 1:39, 1:16, 1:2]


# ------------------------------ create list obj. for modellig
data <- list(
  date = df$date,
  date_numeric = df$date_numeric,
  image = imgs_train,
  month = df$month,
  day = df$day,
  year = df$year,
  season_num = df$season_num,
  season = df$season,
  lag_1 = df$lag_1,
  lag_2 = df$lag_2,
  lag_3 = df$lag_3,
  lag_4 = df$lag_4,
  lag_5 = df$lag_5,
  lag_6 = df$lag_6,
  lag_7 = df$lag_7,
  lag_8 = df$lag_8,
  lag_9 = df$lag_9,
  lag_10 = df$lag_10,
  lag_1_num = df$lag_1_num,
  lag_2_num = df$lag_2_num,
  lag_3_num = df$lag_3_num,
  gwl = df$value
)
str(data)
# ------------------------------ create specific format for target var
y <- to_categorical(as.numeric(as.factor(data$gwl))-1)

# ------------------------------ Create Train/Test-Data for CV
indcs_og <- c(1:length(data$date))
indcs_ts <- createTimeSlices(indcs_og, initialWindow = 10*365, horizon = 1*365, skip = 11*365, fixedWindow = T)
indcs_final <- list(c(1:10))
# create list for deepregression cv indices
for(i in c(1:10)){
  indcs_final[[i]] <- list(indcs_ts$train[[i]], indcs_ts$test[[i]])
  print(round(table(data$gwl[indcs_ts$train[[i]]])/length(data$gwl[indcs_ts$train[[i]]]), 4))
}

# ------------------------------- Create class weights via inverse class frequency
unique_classes <- levels(data$gwl)
class_weigths <- list(c(1:length(unique_classes)))

for (i in seq_along(unique_classes)) {
  current_class <- unique_classes[i]
  n_all <- length(data$gwl)
  nmb_classes <- length(unique_classes)
  sum_current <- length(data$gwl[data$gwl == current_class])
  #class_weigths[[i]] <- sum_current/length(data$gwl)
  class_weigths[[i]] <- n_all/(nmb_classes*sum_current)
}
names(class_weigths) <- unique_classes
