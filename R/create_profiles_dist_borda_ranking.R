# keep_winner_elements <- function(por) {
#   n <- length(por$candidates)
#   m <- sum(por$numberOfVoters)
#   b <- as.numeric(borda(por))
#   boolean_matrix <- matrix(F, nrow = n, ncol = n)
#   # from worst to best
#   for(i in 4:1) {
#     # get the alternative that is in position i
#     alt <- which(b == i)
#     # set the rows of the matrix to TRUE and the column to FALSE
#     # in this way the alternative in the last position get overwritten by
#     # the others
#     boolean_matrix[alt, ] <- T
#     boolean_matrix[, alt] <- F
#   }
#   return(boolean_matrix)
# }

keep_winner_elements <- function(por) {
  n <- length(por$candidates)
  m <- sum(por$numberOfVoters)
  v <- votrix(por)
  b <- as.numeric(borda(por))
  boolean_matrix <- matrix(F, nrow = n, ncol = n)
  boolean_vector <- vector(mode = "numeric", length = (n^2 - n)/2)
  start <- 1
  # from worst to best
  for(i in 4:1) {
    # get the alternative that is in position i
    alt <- which(b == i)
    # set the rows of the matrix to TRUE and the column to FALSE
    # in this way the alternative in the last position get overwritten by
    # the others
    boolean_matrix[alt, ] <- T
    boolean_matrix[, alt] <- F
  }
  for(i in 1:3) {
    alt <- which(b == i)
    elements <- v[alt,][boolean_matrix[alt, ]]
    # print(elements)
    # print(start:(start+length(elements)-1))
    boolean_vector[start:(start+length(elements)-1)] <- elements
    start <- start+length(elements)
  }
  return(boolean_vector)
}

create_profiles <- function(n, nvotes, total = 100, 
                               seed = NULL, 
                               max_distinct = NULL,
                               distribution = "norm",
                               allow_condorcet = FALSE)  {
  
  # Create a list for the profiles
  profiles <- vector(mode = "list", length = total)
  names(profiles) <- paste0("pr",1:total)
  
  # Create a list for the scores sorted by the position of the candidate
  scores <- vector(mode = "list", length = total)
  names(scores) <- paste0("pr",1:total)
  
  # Create a list for the elements
  elements <- vector(mode = "list", length = total)
  names(elements) <- paste0("pr",1:total)
  
  created <- 1
  if(!is.null(seed)) set.seed(seed) # for reproducibility
  if(is.null(max_distinct)) max_distinct <- min(nvotes, factorial(n)) # number of votes
  if(max_distinct > nvotes) stop("max_distinct must be < than nvotes")
  
  while(created <= total) {
    d <- sample(1:max_distinct, 1)
    r <- random_profile_of_rankings(n, 
                                    nvotes, 
                                    distinct = d, 
                                    distribution = distribution)
    b <- as.numeric(borda(r))
    # cat("Borda\n")
    # print(b)
    if(all(1:4 %in% b)) {
      if(!allow_condorcet) {
        # cat("Condorcet\n")
        # print(condorcet(r))
        if(!condorcet(r)) { 
          profiles[[created]] <- r
          v <- votrix(r)
          scores[[created]] <- rowSums(v)[order(b)]
          elements[[created]] <-  keep_winner_elements(r)
          cat(paste("--", created, "\n"))
          created <- created + 1
        }
      } else {
        # only condorcet profiles
        if(condorcet(r)) {
          profiles[[created]] <- r
          v <- votrix(r)
          scores[[created]] <- rowSums(v)[order(b)]
          elements[[created]] <-  keep_winner_elements(r)
          cat(paste("--", created, "\n"))
          created <- created + 1
        }
      }
    }
    
  }
  return(list(profiles, scores, elements))
}

ncandidates <- 4
nvoters <- seq(10,500,10)
#nvoters <- 10
for(i in ncandidates) {
  for(j in nvoters) {
    cat(paste0("Candidatos ", i, ", votantes ", j, "\n"))
    name <- paste("pr4", j, sep = "_")
    p <- create_profiles(n = i, 
                         nvotes = j, 
                         total = 500, 
                         allow_condorcet = FALSE)
    # assign(paste0("profiles_", name), p$profiles)
    # assign(paste0("scores_", name), p$scores)
    # assign(paste0("elements_", name), p$elements)
    # assign(paste0("distances_", name), p$distances)
    save(p, file = paste0(name, ".RData"))
  }
}



resume <- p[[2]] %>% 
  map_dfr(~ .x %>% as_tibble(), .id = "profile") %>% 
  mutate(pos = row_number()%%4,
         pos = ifelse(pos == 0, 4, pos),
         profile = as.factor(profile),
         pos = as.factor(pos))

# resume %>% filter(pos == 1) %>%
#   group_by(value) %>%
#   count()
# 
# resume %>% filter(pos == 2) %>%
#   group_by(value) %>%
#   count()
# 
# resume %>% filter(pos == 3) %>%
#   group_by(value) %>%
#   count()
# 
# resume %>% filter(pos == 4) %>%
#   group_by(value) %>%
#   count()

n <- 4
m <- sum(p[[1]]$pr1$numberOfVoters)
ggplot(resume, aes(value)) +
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 3*m/4) + 
  geom_vline(xintercept = 2*3*m/4) + 
  geom_vline(xintercept = 3*3*m/4) +
  geom_vline(xintercept = 3*m) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-.5, size = 2) +
  facet_wrap(~pos, ncol = 1) +
  scale_x_continuous(limits = c(-1,3*m+1), breaks = seq(0,3*m,1)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())

# histograma
resume <- p[[2]] %>% 
  map_dfr(~ .x %>% as_tibble(), .id = "profile") %>% 
  mutate(pos = row_number()%%4,
         pos = ifelse(pos == 0, 4, pos),
         profile = as.factor(profile),
         pos = as.factor(pos))
n <- 4
m <- sum(p[[1]]$pr1$numberOfVoters)
ggplot(resume, aes(value)) +
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 3*m/4) + 
  geom_vline(xintercept = 2*3*m/4) + 
  geom_vline(xintercept = 3*3*m/4) +
  geom_vline(xintercept = 3*m) +
  geom_histogram(bins = 100) + 
  #geom_text(stat='count', aes(label=..count..), vjust=-.5, size = 2) +
  facet_wrap(~pos, ncol = 1) +
  scale_x_continuous(limits = c(-1,3*m+1), breaks = c(0,3*m/4,2*3*m/4,3*3*m/4,3*m)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())

# ggplot(resume, aes(value, value)) +
#   geom_point() 

resume_wide <- resume %>% pivot_wider(names_from = pos, names_prefix = "pos", values_from = value)
ggplot(resume_wide, aes(pos1, pos2)) +
  geom_point()
ggplot(resume_wide, aes(pos1, pos3)) +
  geom_point()

# Plot de las correlaciones
limitRange <- function(data, mapping, ...) { 
  ggplot(data = data, mapping = mapping, ...) + 
    geom_point(..., alpha = 0.2) + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_vline(xintercept = 15) +
    geom_hline(yintercept = 15) +
    scale_y_continuous(limits = c(-1, 31)) +
    scale_x_continuous(limits = c(-1, 31))
}
ggpairs(resume_wide[,-1], lower = list(continuous = limitRange))

unos_pocos <- resume %>% slice(1:20)
ggplot(unos_pocos, aes(pos,value)) +
  geom_bar(stat = "identity") +
  geom_text(stat='identity', aes(label=value), vjust=-.5) +
  geom_hline(yintercept = 15) +
  facet_grid(profile~.)
  
resume_wide %>% mutate(winners = pos1+pos2,
                       sePasan = pos1+pos2-30,
                       sobran = 30-sePasan,
                       vaya = 30-pos1-pos2+30
                       # difWinners = pos1-pos2,
                       # dif3m = 30-winners,
                       # losers = pos3+pos4,
                       # difLosers = pos3-pos4
                       )

ggplot(
tibble(n = 4:16) %>%
  mutate(greater_than = n^2/8,
         max = (n^2-n)/2) %>%
  pivot_longer(-n), aes(n, value, color = name)) +
  geom_line() +
  geom_point() 
