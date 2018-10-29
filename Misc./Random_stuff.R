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

ggplot(whoop_edit4, aes(x = restHR)) + 
  geom_histogram(binwidth = 2, aes(y = ..density..), color = "black", fill = "grey") + 
  stat_function(fun = dnorm, color = "red",
                args = list(mean = mean(whoop_edit4$restHR, na.rm = TRUE),
                            sd = sd(whoop_edit4$restHR, na.rm = TRUE))) +
  labs(title = "RHR Histogram", x = "Resting Heart Rate")

ggplot(whoop_edit4, aes(x = restHR, y = recovery)) +
  geom_point(alpha = 0.8, color = "red", shape = 5) + 
  labs(title = "Resting Heart Rate vs. Recovery", x = "RHR", y = "Recovery Score")

ggplot(whoop_df_explore, aes(x = strain)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, color="black", fill="grey") + 
  geom_density(aes(x = strain), alpha=.2, fill="#FF6666") +
  labs(title="Strain Histogram", x="Strain Score")

ggplot(whoop_edit4, aes(x = cal, y = strain)) +
  geom_point(alpha = 0.8, color = "red", shape = 5) + 
  labs(title = "Calories vs. Strain Scatterplot", x = "Calories", y = "Strain Score")

ggplot(whoop_edit4, aes(x = sleepPerform)) + 
  geom_histogram(binwidth = 0.05, aes(y = ..density..), color = "black", fill = "grey") + 
  stat_function(fun = dnorm, color = "red",
                args = list(mean = mean(whoop_edit4$sleepPerform, na.rm = TRUE),
                            sd = sd(whoop_edit4$sleepPerform, na.rm = TRUE))) +
  labs(title = "Sleep Performance Histogram", x = "Sleep Performance")

ggplot(whoop_edit4, aes(x = timeInBed)) + 
  geom_histogram(binwidth = 1, aes(y = ..density..), color = "black", fill = "grey") + 
  stat_function(fun = dnorm, color = "red",
                args = list(mean = mean(whoop_edit4$timeInBed, na.rm = TRUE),
                            sd = sd(whoop_edit4$timeInBed, na.rm = TRUE))) + 
  labs(title = "Time in Bed Histogram", x = "Time in Bed")


ggplot(whoop_edit4, aes(x = totalSleep)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..density..), color = "black", fill = "grey") + 
  stat_function(fun = dnorm, color = "red",
                args = list(mean = mean(whoop_edit4$totalSleep, na.rm = TRUE),
                            sd = sd(whoop_edit4$totalSleep, na.rm = TRUE))) +
  labs(title = "Total Sleep Histogram", x = "Total Sleep")

ggplot(whoop_edit4, aes(x = totalSleep)) + 
  geom_histogram(binwidth = 0.5, aes(y = ..density..), color = "black", fill = "grey") + 
  geom_density(aes(x = strain), alpha=.2, fill="#FF6666") +
  labs(title="Total Sleep Histogram", x="Total Sleep")
  