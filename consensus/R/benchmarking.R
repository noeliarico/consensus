library(clusterlab)
set.seed(2019)
data <- clusterlab(centers = 3, r = 1,
                   sdvec = c(0.5,0.5,1),
                   alphas = c(1, 2, 3),
                   numbervec = c(20, 20, 20),
                   features = 2)
points <- t(data$synthetic_data)

dpor <- data.frame()
for(pvalue in seq(1, 1000, 5)){
  distances <- ranking(as.matrix(dist(points, method = "minkowski", p = pvalue))[1, -1])
  dpor <- rbind(dpor, distances)
}
por <- profile_of_rankings(as.matrix(dpor))

benchmark(
  "r" = {scoring(por, method = "borda")},
  "c" = {scoringc(por, method = "borda")}
)
