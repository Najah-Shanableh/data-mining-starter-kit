require(ggplot2)
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
