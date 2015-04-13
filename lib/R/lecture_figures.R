# Linear regression figures
library(dplyr)
library(ggplot2)
library(lubridate)

setwd('~/projects/BI-TECH-CP303/projects/project 1')
usage = read.delim('./data/usage_2012.tsv',
                   sep = '\t',
                   header = TRUE)

stations = read.delim('./data/stations.tsv',
                      sep = '\t',
                      header = TRUE)

weather = read.delim('./data/daily_weather.tsv',
                     sep = '\t',
                     header = TRUE)


mean_custs_per_day = custs_per_day %>% 
  group_by(station_start) %>% 
  summarize(mean_rentals = mean(no_rentals))
head(mean_custs_per_day)
x = merge(mean_custs_per_day, stations, by.x = 'station_start', by.y = 'station')

ggplot(x, aes(x = crossing, y = mean_rentals)) +
  geom_smooth(method = 'lm', size = 2) +
  geom_point(size = 5, alpha = 0.60) +
  theme_minimal() +
  scale_x_continuous('Number of crosswalks within a quarter mile') +
  scale_y_continuous('Number of rentals') +
  theme(
    text = element_text(family = 'Neuton'),
    title = element_text(size = 12),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.title = element_text(size = 30))

plot_model = lm(mean_rentals ~ crossing, data = x)
new_data = data.frame(
  crossing = x$crossing,
  mean_rentals = x$mean_rentals,
  mean_pred = predict(plot_model, x))
new_data$resid = round(with(new_data, mean_rentals - mean_pred), 2)

# Residuals plot
ggplot(x, aes(x = crossing, y = mean_rentals)) +
  geom_linerange(data = new_data, aes(x = crossing, ymin = mean_pred, ymax = mean_rentals), alpha = 0.70, color = 'red3') +
  geom_smooth(method = 'lm', size = 2) +
  geom_point(size = 5, alpha = 0.60) +
  theme_minimal() +
  scale_x_continuous('Number of crosswalks within a quarter mile') +
  scale_y_continuous('Number of rentals') +
  theme(
    text = element_text(family = 'Neuton'),
    title = element_text(size = 12),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.title = element_text(size = 30))

regplot + geom_linerange(data = new_data, 
                         aes(x = crossing, ymin = mean_pred, ymax = mean_rentals), colour = "blue", alpha = 0.40) +
  geom_text(data = new_data, aes(x = crossing, y = mean_rentals, label = resid), hjust = -0.2, colour = "blue")

# Different models plot
coefs = data.frame(a = c(10, 20, 40),
                   b = c(0.50, 0.70, 0.40),
                   color = c(1, 2, 3))

ggplot(x, aes(x = crossing, y = mean_rentals)) +
  geom_abline(data = coefs, aes(intercept = a, slope = b, color = factor(color)), size = 3, alpha = 0.90) + 
  geom_point(size = 5, alpha = 0.60) +
  theme_minimal() +
  scale_x_continuous('Number of crosswalks within a quarter mile') +
  scale_y_continuous('Number of rentals') +
  theme(
    text = element_text(family = 'Neuton'),
    title = element_text(size = 12),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.title = element_text(size = 30))

# model code 
model = lm(rentals ~ crossing, data = data)
summary(model)

plot_model = lm(mean_rentals ~ crossing, data = x)



# Logistic regression example figure

df_1 = data.frame(class = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
                count = c(1, 3, 4, 5, 6, 9, 10, 11, 13, 15))

# Dimensions of this plot in presentation are: 749 × 642
ggplot(df_1, aes(x = count, y = class)) +
  geom_hline(yintercept = 0.50, linetype = 2, alpha = 0.50, size = 1.5) +
  geom_vline(xintercept = 7.699844, linetype = 2, alpha = 0.50, size = 1.5, color = '#e7ad52') +
  geom_line(stat = 'smooth', method = lm, se = FALSE, size = 3, alpha = 0.20) +
  geom_point(size = 10, color = 'Darkslategray') +
  theme_minimal() +
  scale_x_continuous("Number of occurrences of $$$") +
  scale_y_continuous("Is it spam?", breaks = c(0, 1), labels = c(0, 1)) +
  theme(
    text = element_text(family = 'Neuton'),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.title = element_text(size = 30))

df_2 = data.frame(class = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                count = c(1, 3, 4, 5, 6, 9, 10, 11, 13, 15, 40))

# Dimensions of this plot in presentation are: 749 × 642
ggplot(df_2, aes(x = count, y = class)) +
  geom_hline(yintercept = 0.50, linetype = 2, alpha = 0.50, size = 1.5) +
  geom_vline(xintercept = 7.699844, linetype = 2, alpha = 0.50, size = 1.5, color = '#e7ad52') +
  geom_vline(xintercept = 9.122918, linetype = 2, alpha = 0.50, size = 1.5, color = '#e7ad52') +
  geom_line(stat = 'smooth', method = lm, se = FALSE, size = 3, alpha = 0.20) +
  geom_line(data = df_1, stat = 'smooth', method = lm, se = FALSE, size = 3, alpha = 0.20) +
  geom_point(size = 10, color = 'Darkslategray') +
  theme_minimal() +
  scale_x_continuous("Number of occurrences of $$$") +
  scale_y_continuous("Is it spam?", breaks = c(0, 1), labels = c(0, 1)) +
  theme(
    text = element_text(family = 'Neuton'),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.title = element_text(size = 30))











