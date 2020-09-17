kemeny_times <- tribble(~nc, ~parallel, ~time,
                        4, FALSE, t4a['elapsed'],
                        4, TRUE, t4b['elapsed'],
                        5, FALSE, t5a['elapsed'],
                        5, TRUE, t5b['elapsed'],
                        6, FALSE, t6a['elapsed'],
                        6, TRUE, t6b['elapsed'],
                        7, FALSE, t7a['elapsed'],
                        7, TRUE, t7b['elapsed'],
                        8, FALSE, t8a['elapsed'],
                        8, TRUE, t8b['elapsed'],
                        9, FALSE, t9a['elapsed'],
                        9, TRUE, t9b['elapsed'],
                        10, FALSE, t10a['elapsed'],
                        10, TRUE, t10b['elapsed']
                        )


ggplot(kemeny_times %>% filter(nc < 8), aes(nc, time, color = parallel)) +
  geom_line() +
  geom_point()

ggplot(kemeny_times, aes(nc, time, color = parallel)) +
  geom_line() +
  geom_point()