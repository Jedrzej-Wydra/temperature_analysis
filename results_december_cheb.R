library(readr)
library(dplyr)
library(ggplot2)


data_set <- read_csv('bootstrap_december_cheb.csv')
head(data_set)

# linear variable is easier to interpret than quadratic one

data_set <- data_set %>% 
  mutate(error = sqrt(error),
         Best_error = sqrt(Best_error),
         Worst_error = sqrt(Worst_error))
head(data_set)


data_set_results <- data_set %>% 
  mutate(better = error < Worst_error) %>%
  group_by(location, number) %>% 
  summarise(percent = 100 * sum(better) / length(better))

head(data_set_results)

ggplot(data_set_results, aes(number, percent)) +
  geom_line() +
  facet_wrap(~location) +
  geom_hline(yintercept = 50) +
  scale_x_continuous(breaks = 2:12) +
  theme_bw() +
  ggtitle("December")

################################################################################

data_set_results_2 <- data_set %>%
  group_by(location, number) %>% 
  summarise(error = mean(error), Best_error = mean(Best_error)) %>% 
  mutate(im_error = c(0, diff(error)) / Best_error)


ggplot(data_set_results_2, aes(number, im_error)) +
  geom_line() +
  scale_x_continuous(breaks = 2:12) +
  facet_wrap(~location) +
  theme_bw() +
  ggtitle("December")

################################################################################

data_set_results_3 <- data_set %>%
  group_by(location, number) %>% 
  summarise(error = mean(error),
            Best_error = mean(Best_error),
            Worst_error = mean(Worst_error))

ggplot(data_set_results_3, aes(number, error)) +
  geom_line() +
  scale_x_continuous(breaks = 2:12) +
  geom_line(aes(number, Best_error), color = "red") +
  geom_line(aes(number, Worst_error), color = "blue") +
  facet_wrap(~location) +
  theme_bw() +
  ggtitle("December")

################################################################################
