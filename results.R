library(readr)
library(dplyr)
library(ggplot2)

data_set_august <- read_csv('bootstrap_august.csv')
head(data_set_august)
data_set_august_results <- data_set_august %>% 
  mutate(better = error < Worst_error) %>%
  group_by(location, distance, number) %>% 
  summarise(percent = 100* sum(better) / length(better))
head(data_set_august_results)

ggplot(data_set_august_results, aes(number, percent, color = as.factor(distance))) +
  geom_line() +
  facet_wrap(~location) +
  geom_vline(xintercept = 3, color = "red") +
  geom_vline(xintercept = 5, color = "blue") +
  geom_hline(yintercept = 50) +
  scale_x_continuous(breaks = 2:12) +
  theme_bw()

# Think about it
################################################################################

data_set_august_results_2 <- data_set_august %>%
  group_by(location, number) %>% 
  mutate(im_error = 100 * diff(error) / Best_error) %>%
  filter(number > 2) %>% 
  summarise(im_error = mean(im_error))

ggplot(data_set_august_results_2, aes(number, im_error, color = location)) +
  geom_line() +
  scale_x_continuous(breaks = 2:12) +
  theme_bw()

################################################################################

data_set_august_results_3 <- data_set_august %>% 
  group_by(location, number) %>% 
  filter(number > 2) %>% 
  summarise(error = 100 * sqrt(mean(error)) / sqrt(max(Best_error)))

ggplot(data_set_august_results_3, aes(number, error, color = location)) +
  geom_line() +
  scale_x_continuous(breaks = 2:12) +
  theme_bw()

