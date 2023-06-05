library(ggplot2)

# ------------------------------- Histogram für Zielvariable -- Verteilung
hist_tar <- ggplot(df, aes(x=value)) +
  geom_histogram(stat = "count")

hist_tar_season <- ggplot(df, aes(x=value, color=season)) +
  geom_histogram(stat = "count")

# Rate evolution graph
# create df for each cat
# x = nmb of events bzw date,
# y = value

for (i in c(1:length(unique_classes))) {
  tmp_df = df[df$value == unique_classes[i], ]
  tmp_df$count = c(1:nrow(tmp_df))
  assign(paste0("df_", rel_gwl[i]), tmp_df)
}


ggplot(data = df, aes(x = date)) +
  geom_line(data = get(paste0(paste0("df_", rel_gwl[1]))), aes(x = date, y = count)) +
  geom_point(data = get(paste0(paste0("df_", rel_gwl[1]))), aes(x = date, y = count)) +
  geom_line(data = get(paste0(paste0("df_", rel_gwl[2]))), aes(x = date, y = count)) +
  geom_point(data = get(paste0(paste0("df_", rel_gwl[2]))), aes(x = date, y = count)) +
  geom_line(data = get(paste0(paste0("df_", rel_gwl[3]))), aes(x = date, y = count)) +
  geom_point(data = get(paste0(paste0("df_", rel_gwl[3]))), aes(x = date, y = count)) +
  geom_line(data = get(paste0(paste0("df_", rel_gwl[4]))), aes(x = date, y = count)) +
  geom_point(data = get(paste0(paste0("df_", rel_gwl[4]))), aes(x = date, y = count)) +
  geom_line(data = get(paste0(paste0("df_", rel_gwl[5]))), aes(x = date, y = count)) +
  geom_point(data = get(paste0(paste0("df_", rel_gwl[5]))), aes(x = date, y = count))


# Visualisierung der Folds
