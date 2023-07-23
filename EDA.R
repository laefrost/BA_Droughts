library(ggplot2)
library(rcompanion)
library(RColorBrewer)

source("preprocessing.R")
# ------------------------------- Setting colors etc.
colours_classes <- palette.colors(palette = "Okabe-Ito")[2:9]
viz_years_breaks <- c("1900", "1910", "1920", "1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000", "2010")
viz_ordered_classes <- c("BM", "HFA", "HNA", "HNFA", "NEA", "SEA", "other")
palette_gwls <- brewer.pal(n = 7, name = "Dark2")
# ------------------------------- Plots of circulation pattern distribution
# Histogram for absolute frequency
ggplot(df, aes(x=value)) +
  geom_bar(fill = palette_gwls) +
  scale_x_discrete(name = "Circulation pattern") +
  scale_y_continuous(name = "Absolute frequency", breaks = seq(2000, 32000, by = 5000))


# Bar plot for distribution over time
eda_df_year <- df %>% group_by(year, value) %>% count()
ggplot(eda_df_year, aes(fill=value, y=n, x= as.factor(year))) +
  geom_bar(position="fill", stat="identity", width = 1) +
  scale_x_discrete(name = "Year", breaks = viz_years_breaks) +
  scale_y_continuous(name = "Relative frequency") +
  #scale_fill_discrete(name = "Circulation patterns", breaks=viz_ordered_classes) +
  scale_fill_brewer(palette=palette_gwls, name = "Circulation patterns", breaks=viz_ordered_classes) +
  theme(legend.position = "bottom") + guides(fill = guide_legend(nrow = 1))



# Barplots for avg. distribution over year
tmp_df_season <- df %>% group_by(year, season, value) %>%
  count() %>% group_by(season,value) %>%
  dplyr::summarize(Mean = mean(n, na.rm=TRUE)) +
  scale_fill_viridis_d() +

ggplot(tmp_df_season, aes(fill=value, y=Mean, x= as.factor(season))) +
  geom_bar(position="fill", stat="identity")


#tmp_df_season <- df %>% mutate(decade = year %% 10) %>% group_by(decade, month, value) %>% count()#%>% dplyr::summarize(Mean = mean(n, na.rm=TRUE))
#tmp_df_ooc <- df %>% group_by(year, month, value) %>% mutate(value = value) %>% count()
#
# for(i in 0:(length(unique(tmp_df_season$decade))-1)) {
#   print(i)
#   print(ggplot(tmp_df_season[tmp_df_season$decade == i,], aes(fill=value, y=n, x= as.factor(month))) +
#     geom_bar(position="fill", stat="identity"))
# }
#
# tmp_df_month <- df %>% group_by(month, value) %>% count()
#
# ggplot(tmp_df_month, aes(fill=value, y=n, x= as.factor(month))) +
#   geom_bar(position="fill", stat="identity")
#
# ggplot(tmp_df_season, aes(fill=value, y=Mean, x= as.factor(month))) +
#   geom_bar(position="fill", stat="identity")
#
# ggplot(tmp_df, aes(fill=value, y=n, x= as.factor(year))) +
#   geom_bar(position="fill", stat="identity")
#
# print(tmp_df, n = 100)


# Cramers V
df_cramer <- df
for (i in c(1:365)){
  varname <- paste0("lag_",i)
  df_cramer <- df_cramer %>% mutate("lag_{i}" := lag(value, i))
}
cramers <- data.frame("lag" = c(1:365), "cramersV" = c(1:365))
for (i in c(1:365)) {
  cramers[i, "cramersV"] <- cramerV(table(df_cramer[, c("value", paste0("lag_", i))]))
}

ggplot(data=cramers, aes(x=lag, y=cramersV)) +
  geom_bar(stat="identity")

# Rate evolution graph
for (i in c(1:length(unique_classes))) {
  tmp_df = df[df$value == unique_classes[i], ]
  tmp_df$count = c(1:nrow(tmp_df))
  assign(paste0("df_", unique_classes[i]), tmp_df)
}

ggplot(data = df, aes(x = as.Date(date))) +
  #geom_line(data = get(paste0(paste0("df_", unique_classes[1]))), aes(x = date, y = count), color = "green") +
  #geom_point(data = get(paste0(paste0("df_", unique_classes[1]))), aes(x = date, y = count)) +
  geom_line(data = get(paste0(paste0("df_", unique_classes[2]))), aes(x = date, y = count, colour = unique_classes[2])) +
  geom_point(data = get(paste0(paste0("df_", unique_classes[2]))), aes(x = date, y = count, colour = unique_classes[2])) +
  geom_line(data = get(paste0(paste0("df_", unique_classes[3]))), aes(x = date, y = count, colour = unique_classes[3])) +
  geom_point(data = get(paste0(paste0("df_", unique_classes[3]))), aes(x = date, y = count, colour = unique_classes[3])) +
  geom_line(data = get(paste0(paste0("df_", unique_classes[4]))), aes(x = date, y = count, colour = unique_classes[4])) +
  geom_point(data = get(paste0(paste0("df_", unique_classes[4]))), aes(x = date, y = count, colour = unique_classes[4])) +
  geom_line(data = get(paste0(paste0("df_", unique_classes[5]))), aes(x = date, y = count, colour = unique_classes[5])) +
  geom_point(data = get(paste0(paste0("df_", unique_classes[5]))), aes(x = date, y = count, colour = unique_classes[5])) +
  geom_line(data = get(paste0(paste0("df_", unique_classes[6]))), aes(x = date, y = count, colour = unique_classes[6])) +
  geom_point(data = get(paste0(paste0("df_", unique_classes[6]))), aes(x = date, y = count, colour = unique_classes[6])) +
  geom_line(data = get(paste0(paste0("df_", unique_classes[7]))), aes(x = date, y = count, colour = unique_classes[7])) +
  geom_point(data = get(paste0(paste0("df_", unique_classes[7]))), aes(x = date, y = count, colour = unique_classes[7])) +
  scale_x_date(name = "Year", breaks = seq(as.Date("1900-01-01"), as.Date("2010-12-31"), by="10 years"), date_labels = "%Y") + scale_y_discrete(name = "Culmulative frequency") +
  scale_colour_manual(name = "Circulation pattern", values = palette_gwls[2:7])

