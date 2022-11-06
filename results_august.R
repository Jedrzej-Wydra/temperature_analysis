library(readr)
library(dplyr)
library(ggplot2)


data_set <- read_csv('bootstrap_august.csv')
head(data_set)

# linear variable is easier to interpret than quadratic one

data_set <- data_set %>% 
  mutate(error = sqrt(error),
         Best_error = sqrt(Best_error),
         Worst_error = sqrt(Worst_error))
head(data_set)


data_set_results <- data_set %>% 
  mutate(better = error < Worst_error) %>%
  group_by(location, distance, number) %>% 
  summarise(percent = 100 * sum(better) / length(better))

head(data_set_results)

ggplot(data_set_results, aes(number, percent, color = as.factor(distance))) +
  geom_line() +
  facet_wrap(~location) +
  geom_vline(xintercept = 3, color = "red") +
  geom_vline(xintercept = 5, color = "blue") +
  geom_hline(yintercept = 50) +
  scale_x_continuous(breaks = 2:12) +
  theme_bw() +
  ggtitle("August")

################################################################################

data_set_results_2 <- data_set %>%
  group_by(location, distance, number) %>% 
  summarise(error = mean(error), Best_error = mean(Best_error)) %>% 
  mutate(im_error = c(0, diff(error)) / Best_error)
  

ggplot(data_set_results_2, aes(number, im_error, color = as.factor(distance))) +
  geom_line() +
  scale_x_continuous(breaks = 2:12) +
  facet_wrap(~location) +
  theme_bw() +
  ggtitle("August")

ggplot(data_set_results_2, aes(distance, im_error, color = as.factor(number))) +
  geom_line() +
  facet_wrap(~location) +
  theme_bw() +
  ggtitle("August")

################################################################################

data_set_results_3 <- data_set %>%
  group_by(location, distance, number) %>% 
  summarise(error = mean(error))

ggplot(data_set_results_3, aes(number, error, color = as.factor(distance))) +
  geom_line() +
  scale_x_continuous(breaks = 2:12) +
  facet_wrap(~location) +
  theme_bw() +
  ggtitle("August")

################################################################################

ggplot(data_set_results, aes(distance, percent, color = as.factor(number))) +
  geom_line() +
  facet_wrap(~location) +
  geom_hline(yintercept = 50) +
  theme_bw() +
  ggtitle("August")

################################################################################

data_set_measurements_4_dist_12 <- subset(data_set,
                                          data_set$number == 4 &
                                                 data_set$distance == 12)

(data_set_measurements_4_dist_12 %>% 
  group_by(location) %>% 
  mutate(better = error < Worst_error) %>% 
  summarise(percent = 100 * sum(better) / length(better)) -> august)
