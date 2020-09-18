set.seed(123)
print("4")
r <- random_profile_of_rankings(4, 4)
t4a <- system.time(kemeny(r))
t4b <- system.time(kemeny(r, parallel = TRUE))
print("5")
r <- random_profile_of_rankings(5, 4)
t5a <- system.time(kemeny(r))
t5b <- system.time(kemeny(r, parallel = TRUE))
print("6")
r <- random_profile_of_rankings(6, 4)
t6a <- system.time(kemeny(r))
t6b <- system.time(kemeny(r, parallel = TRUE))
print("7")
r <- random_profile_of_rankings(7, 4)
t7a <- system.time(kemeny(r))
t7b <- system.time(kemeny(r, parallel = TRUE))
print("8")
r <- random_profile_of_rankings(8, 4)
t8a <- system.time(kemeny(r))
t8b <- system.time(kemeny(r, parallel = TRUE))
print("9")
r <- random_profile_of_rankings(9, 4)
t9a <- system.time(kemeny(r))
t9b <- system.time(kemeny(r, parallel = TRUE))
print("10")
r <- random_profile_of_rankings(10, 4)
t10a <- system.time(kemeny(r))
t10b <- system.time(kemeny(r, parallel = TRUE))

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

save(kemeny_times, file = "kemeny_times")

# Plot until parallel gets better than sequential
ggplot(kemeny_times %>% filter(nc < 8), aes(nc, time, color = parallel)) +
  geom_line() +
  geom_point()

# Plot all
ggplot(kemeny_times, aes(nc, time, color = parallel)) +
  geom_line() +
  geom_point()