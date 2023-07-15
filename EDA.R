library(ggplot2)
library(rcompanion)
source("preprocessing.R")
# ------------------------------- Histogram für Zielvariable -- Verteilung
hist_tar <- ggplot(df, aes(x=value)) +
  geom_histogram(stat = "count")

hist_tar_season <- ggplot(df, aes(x=value, color=season)) +
  geom_histogram(stat = "count")


tmp_df <- df %>% group_by(year, value) %>% count()

tmp_df_season <- df %>% group_by(year, month, value) %>% count() %>% group_by(month,value) %>%
  dplyr::summarize(Mean = mean(n, na.rm=TRUE))


tmp_df_season <- df %>% mutate(decade = year %% 10) %>% group_by(decade, month, value) %>% count()#%>% dplyr::summarize(Mean = mean(n, na.rm=TRUE))
#tmp_df_ooc <- df %>% group_by(year, month, value) %>% mutate(value = value) %>% count()

for(i in 0:(length(unique(tmp_df_season$decade))-1)) {
  print(i)
  print(ggplot(tmp_df_season[tmp_df_season$decade == i,], aes(fill=value, y=n, x= as.factor(month))) +
    geom_bar(position="fill", stat="identity"))
}

tmp_df_month <- df %>% group_by(month, value) %>% count()

ggplot(tmp_df_month, aes(fill=value, y=n, x= as.factor(month))) +
  geom_bar(position="fill", stat="identity")

ggplot(tmp_df_season, aes(fill=value, y=Mean, x= as.factor(month))) +
  geom_bar(position="fill", stat="identity")

ggplot(tmp_df, aes(fill=value, y=n, x= as.factor(year))) +
  geom_bar(position="fill", stat="identity")

print(tmp_df, n = 100)

# Cramers V
cramers <- data.frame("lag" = c(1:30), "cramersV" = c(1:30))
cramers_og <- data.frame("lag" = c(1:30), "cramersV" = c(1:30))
cramers_shift <- data.frame("lag" = c(1:2), "cramersV" = c(1:2))

df_g <- na.omit(df)
for (i in c(1:30)) {
  cramers[i, "cramersV"] <- cramerV(table(df[, c("value", paste0("lag_", i))]))
  cramers_og[i, "cramersV"] <- cramerV(table(df[, c("value_og", paste0("lag_", i))]))
}

for (i in c(1:2)) {
  cramers_shift[i, "cramersV"] <- cramerV(table(df_g[, c("value", paste0("shift_", i))]))
}

cramers
cramers_og
cramers_shift

ggplot(data=cramers, aes(x=lag, y=cramersV)) +
  geom_bar(stat="identity")

ggplot(data=cramers_og, aes(x=lag, y=cramersV)) +
  geom_bar(stat="identity")

### Also ab 6 eig nicht merh wichtig











# Rate evolution graph
# create df for each cat
# x = nmb of events bzw date,
# y = value

for (i in c(1:length(unique_classes))) {
  tmp_df = df[df$value == unique_classes[i], ]
  tmp_df$count = c(1:nrow(tmp_df))
  assign(paste0("df_", rel_gwl[i]), tmp_df)
}

# rate evolution graph
ggplot(data = df, aes(x = date)) +
  #geom_line(data = get(paste0(paste0("df_", rel_gwl[1]))), aes(x = date, y = count)) +
  #geom_point(data = get(paste0(paste0("df_", rel_gwl[1]))), aes(x = date, y = count)) +
  geom_line(data = get(paste0(paste0("df_", rel_gwl[2]))), aes(x = date, y = count)) +
  geom_point(data = get(paste0(paste0("df_", rel_gwl[2]))), aes(x = date, y = count)) +
  geom_line(data = get(paste0(paste0("df_", rel_gwl[3]))), aes(x = date, y = count)) +
  geom_point(data = get(paste0(paste0("df_", rel_gwl[3]))), aes(x = date, y = count)) +
  geom_line(data = get(paste0(paste0("df_", rel_gwl[4]))), aes(x = date, y = count)) +
  geom_point(data = get(paste0(paste0("df_", rel_gwl[4]))), aes(x = date, y = count)) +
  geom_line(data = get(paste0(paste0("df_", rel_gwl[5]))), aes(x = date, y = count)) +
  geom_point(data = get(paste0(paste0("df_", rel_gwl[5]))), aes(x = date, y = count)) +
  geom_line(data = get(paste0(paste0("df_", rel_gwl[6]))), aes(x = date, y = count)) +
  geom_point(data = get(paste0(paste0("df_", rel_gwl[6]))), aes(x = date, y = count)) +
  geom_line(data = get(paste0(paste0("df_", rel_gwl[7]))), aes(x = date, y = count)) +
  geom_point(data = get(paste0(paste0("df_", rel_gwl[7]))), aes(x = date, y = count))


# Bilder
imgs[, , 1, 2]


# Visualisierung der Folds
