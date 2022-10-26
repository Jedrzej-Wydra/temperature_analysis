library(ggplot2)

aug_data <- read_csv('bootstrap_august.csv')
head(aug_data)

aug_test_attic <- subset(aug_data, location=='field')

aug_test_attic %>%
  group_by(distance, number) %>%
  summarise(q1 = quantile(error, 0.025),
            q2 = quantile(error, 0.975),
            mean = mean(error),
            distance = distance / 24) %>% 
  filter(q2 < 200000) %>% 
  mutate(distance = as.factor(distance)) %>% 
  ggplot(aes(number)) +
  geom_line(aes(y=q1),color = "red") +
  geom_line(aes(y=q2),color = "green") +
  geom_line(aes(y=mean),color = "blue") +
  geom_hline(yintercept = aug_test_attic$Best_error[1]) +
  geom_hline(yintercept = aug_test_attic$Worst_error[1]) +
  facet_wrap(~distance)
