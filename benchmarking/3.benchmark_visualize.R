ggplot(results %>% mutate(as.factor(matrix)), aes(nc, time, col = as.factor(nr))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ matrix)

ggplot(results %>% mutate(as.factor(matrix)), aes(nr, time, col = as.factor(nc))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ matrix)




