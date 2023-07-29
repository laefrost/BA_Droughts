library(dplyr)
library(ggplot2)
library(mgcv)
library(reshape2)
library(lubridate)
library(nnet)
library(tidyr)
library(longCatEDA)
library(lattice)
library(caret)

# ------------------------------ Load and convert data into time series format (df)
df_og <- read.csv2("GWL_1900-2010.csv", sep = ";")
rel_gwl <- c("BM", "HFA", "HNA", "HNFA", "NEA", "SEA")
df <- df_og

### change format from long  to wide
df <- melt(df, id = "JAHRMONAT")
# create date column
df$day <- as.numeric(gsub("\\D", "", df$variable))
df$day <-
  ifelse(df$day >= 10, as.character(df$day), paste(0, df$day, sep = ""))
df$date <- ymd(paste(df$JAHRMONAT, df$day, sep = ""))

### create additional covariates
df <- df %>% arrange(date) %>% drop_na(date) %>%
  mutate(
    month = as.factor(format(date, "%m")),
    year = as.numeric(format(date, "%Y")),
    value_og = value,
    value = relevel(as.factor(ifelse(value %in% rel_gwl, value, "other")), ref = "other"),
    season = as.factor(
      case_when(
        month %in% c("3", "4", "5") ~ "spring",
        month %in% c("6", "7", "8") ~ "summer",
        month %in% c("9", "10", "11") ~ "fall",
        TRUE ~ "winter"
      )
    ),
    season_num = as.numeric(season),
    lag_1 = relevel(lag(value, n = 1), ref = "other"),
    lag_2 = relevel(lag(value, n = 2), ref = "other"),
    lag_3 = relevel(lag(value, n = 3), ref = "other"),
    lag_4 = relevel(lag(value, n = 4), ref = "other"),
    lag_5 = relevel(lag(value, n = 5), ref = "other"),
    lag_6 = relevel(lag(value, n = 6), ref = "other"),
    date_numeric = as.numeric(date),
    day_year = lubridate::yday(date),
  )

### remove missing day
df <- df[df$date != "1942-11-30", ]

### create shifted vals
df$group_id <- cumsum(df$value != df$lag_1  | is.na(df$lag_1 ))
df_transitions_first <- df %>% group_by(group_id) %>% mutate(transition_first = c(T, rep_len(F, length.out = n()-1)))
df_transitions_last <- df %>% group_by(group_id) %>% mutate(transition_last = c(rep_len(F, length.out = n()-1), T))
df_length <- df %>% group_by(group_id) %>% mutate(curr_length = 1:n())
df$curr_length <- df_length$curr_length
df <- df %>% mutate(curr_length = lag(curr_length, 1))
df$transition_first <- df_transitions_first$transition_first
df$transition_last <- df_transitions_last$transition_last
df <- df %>% mutate(transition = case_when(
  transition_first == T | transition_last == T ~ T,
  T ~ F
))
df_og_length <- df
df <- df[-(1:6), , drop = FALSE]
df_old <- df
df <- df[-c(nrow(df)), ,drop = FALSE]

# ------------------------------ Load and rescale images
imgs <- readRDS("mslp_z500.rds")
imgs_train <- imgs
imgs_train <- array_reshape(imgs_train, c(40541, 39, 16, 2))
imgs_train <- imgs_train/255
# mslp: mean sea level pressure
# z500: Geopotential
# Format: 40541 x 39 x 16 x 2 --> 2 wg. mslp und z500
imgs_train_og <- imgs_train[1:40541, 1:39, 1:16, 1:2]
imgs_train <- imgs_train[7:40540, 1:39, 1:16, 1:2]

# ### create lagged images
# imgs_lagged <- imgs_train_og
# imgs_lagged <- imgs_train_og[6:40539, 1:39, 1:16, 1:2]
# imgs_leaded <- imgs_train_og[8:40541, 1:39, 1:16, 1:2]
#
# imgs_train_combined <- array(numeric(), c(40534, 39, 16, 6))
# imgs_train_combined[,,,1:2] <- imgs_train
# imgs_train_combined[,,,3:4] <- imgs_lagged
# imgs_train_combined[,,,5:6] <- imgs_leaded
#
# imgs_train_lagged <- array(numeric(), c(40534, 39, 16, 4))
# imgs_train_lagged[,,,1:2] <- imgs_train
# imgs_train_lagged[,,,3:4] <- imgs_lagged
#
# imgs_train_lead <- array(numeric(), c(40534, 39, 16, 4))
# imgs_train_lead[,,,1:2] <- imgs_leaded
# imgs_train_lead[,,,3:4] <- imgs_train

# ------------------------------ create list obj. for modellig
data <- list(
  date = df$date,
  date_numeric = df$date_numeric,
  image = imgs_train,
  month = df$month,
  day = df$day,
  year = df$year,
  season = df$season,
  lag_1 = df$lag_1,
  lag_2 = df$lag_2,
  lag_3 = df$lag_3,
  lag_4 = df$lag_4,
  lag_5 = df$lag_5,
  lag_6 = df$lag_6,
  gwl = df$value,
  curr_length = df$curr_length
  #image_lagged = imgs_lagged,
  #image_lead = imgs_leaded,
  #image_combined_lagged_lead = imgs_train_combined,
  #image_combined_lagged = imgs_train_lagged,
  #image_combined_lead = imgs_train_lead
)


# ------------------------------ create specific format for target var
y <- to_categorical(as.numeric(as.factor(data$gwl))-1)

# ------------------------------- Create class weights via inverse class frequency
unique_classes <- levels(data$gwl)
class_weights <- list(c(1:length(unique_classes)))

for (i in seq_along(unique_classes)) {
  current_class <- unique_classes[i]
  print(current_class)
  n_all <- length(data$gwl)
  nmb_classes <- length(unique_classes)
  sum_current <- length(data$gwl[data$gwl == current_class])
  #class_weigths[[i]] <- sum_current/length(data$gwl)
  #class_weights[[i]] <- n_all/(nmb_classes*sum_current)
  class_weights[[i]] <- n_all/(sum_current)

}
names(class_weights) <- c(0:6)
