### Info: Implements EDA for circulation patterns
### Called from: -

library(ggplot2)
library(rcompanion)
library(RColorBrewer)
library(irr)
library(psych)
library(gridExtra)
library(kableExtra)
library(knitr)
source("preprocessing.R")

# ------------------------------- Setting colors etc.
colours_classes <- palette.colors(palette = "Okabe-Ito")[2:9]
viz_years_breaks <- c("1900", "1910", "1920", "1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000", "2010")
viz_ordered_classes <- c("BM", "HFA", "HNA", "HNFA", "NEA", "SEA", "other")
palette_gwls <- c("#6e2474", "#eee009","#ad1313","#60adce","#e925e0","#e3a51d","#95cb77")

# ------------------------------- Plots of circulation pattern distribution
### Histogram for absolute frequency
ggplot(df, aes( x= factor(value, levels = viz_ordered_classes), fill = value)) +
  geom_bar(color = "black", width = 0.8) +
  scale_x_discrete(name = "Circulation pattern") +
  theme(legend.position = "none") +
  scale_fill_manual(values= palette_gwls , name = "Circulation pattern", breaks=viz_ordered_classes) +
  scale_y_continuous(name = "Absolute frequency", breaks = seq(2000, 32000, by = 5000))

### Bar plot for distribution over time
eda_df_year <- df %>% group_by(year, value) %>% count()
ggplot(eda_df_year, aes(fill=value, y=n, x= as.factor(year))) +
  geom_bar(position="fill", stat="identity", width = 1) +
  scale_x_discrete(name = "Year", breaks = viz_years_breaks) +
  scale_y_continuous(name = "Relative frequency") +
  scale_fill_manual(values= palette_gwls , name = "Circulation pattern", breaks=viz_ordered_classes) +
  theme(legend.position = "bottom") + guides(fill = guide_legend(nrow = 1))

### Boxplot for duration
eda_df_length <- df %>% select(-c(variable, season, season_num)) %>% group_by(group_id) %>% mutate(transition_eda = c(rep_len(F, length.out = n()-1), T), curr_length = 1:n()) %>%
  filter(transition_eda == T) #%>% mutate(value = factor(value, levels = viz_ordered_classes))
ggplot(eda_df_length, aes(x = factor(value, levels = viz_ordered_classes), y = curr_length, fill = value)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values= palette_gwls , name = "Circulation pattern", breaks=viz_ordered_classes) +
  theme(legend.position = "none") +
  scale_x_discrete(name = "Circulation pattern") +
  scale_y_continuous(name = "Length of circulation pattern", breaks = seq(0, 80, by = 20)) +
  coord_cartesian(ylim = c(0, 80))

### Cramer's V
df_cramer <- df
for (i in c(1:365)){
  varname <- paste0("lag_",i)
  df_cramer <- df_cramer %>% mutate("lag_{i}" := lag(value, i))
}
cramers <- data.frame("lag" = c(1:365), "cramersV" = c(1:365))
for (i in c(1:365)) {
  cramers[i, "cramersV"] <- cramerV(table(df_cramer[, c("value", paste0("lag_", i))]))
}

plot_cramers_365 <- ggplot(data=cramers, aes(x=lag, y=cramersV)) +
  geom_bar(stat="identity", width = 1) +
  scale_x_continuous(name = "Lag", breaks = seq(0, 360, by = 30)) +
  scale_y_continuous(name = "Cramer's V", breaks = seq(0, 1, by = 0.1)) +
  coord_cartesian(ylim = c(0, 1))

plot_cramers_10 <- ggplot(data=cramers[c(1:10),], aes(x=lag, y=cramersV)) +
  geom_bar(stat="identity", width = 0.5) +
  scale_x_continuous(name = "Lag", breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(name = "Cramer's V", breaks = seq(0, 1, by = 0.1)) +
  coord_cartesian(ylim = c(0, 1))

grid.arrange(plot_cramers_10, plot_cramers_365, ncol=2)


### Rate evolution graph
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
  scale_colour_manual(name = "Circulation pattern", values = palette_gwls[1:6]) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title = "Circulation pattern", nrow = 1))
