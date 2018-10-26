library(ggplot2)

ggplot(whoop_edit4, aes(x = averHR, y = strain)) +
  geom_point(alpha = 0.8, color = "red", fill = "black", shape = 5) + 
  labs(title = "Average Heart Rate vs. Strain Scatterplot", x = "Average Heart Rate", y = "Strain Score")

ggplot(whoop_edit4, aes(x = maxHR, y = strain)) + 
  geom_point(alpha = 0.8, color = "blue", shape = 5) + 
  labs(title = "Max Heart Rate vs. Strain Scatterplot", x = "Max Heart Rate", y = "Strain Score")

ggplot(whoop_edit4, aes(x = cal, y = strain)) +
  geom_point(alpha = 0.8, color = "red", shape = 5) + 
  labs(title = "Calories vs. Strain Scatterplot", x = "Calories", y = "Strain Score")

summary(whoop_edit4)

ggplot(whoop_edit4, aes(x = hrv)) + 
  geom_histogram(binwidth = 5, aes(y = ..density..),  color="black", fill="grey") + 
  stat_function(fun = dnorm, color = "red",
                args = list(mean = mean(whoop_edit4$hrv, na.rm = TRUE),
                            sd = sd(whoop_edit4$hrv, na.rm = TRUE))) +
  labs(title = "HRV Histogram", x = "Heart-Rate Variability")

ggplot(whoop_edit4, aes(x = hrv, y = recovery)) + 
  geom_point(alpha = 0.8, color = "red", shape = 5) + 
  labs(title = "HRV vs. Recovery Scatterplot", x = "HRV", y = "Recovery Score")



