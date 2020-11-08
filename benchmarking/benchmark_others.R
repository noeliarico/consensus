raw <- replicate(100, sample(paste0("C", 1:100)))
raw <- matrix(raw, ncol = 100, byrow = TRUE)
vote <- create_vote(raw, xtype = 2, candidate = paste0("C", 1:100))

system.time(borda_method(vote))['elapsed']
system.time(cdc_schulze(vote))['elapsed']
