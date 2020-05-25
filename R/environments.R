# hashKemeny6 <- new.env()
# perm <- read.csv("perm6.csv")
# perm <- as.por(perm)
# 
# unlist(lapply(1:nrow(perm), function(x){
#   lapply(1:nrow(perm), function(y) {
#     r1 <- get_ranking(perm, x)
#     r2 <- get_ranking(perm, y)
#     #print(r1)
#     #print(r2)
#     name <- getHash(r1, r2)
#     dist <- kendall(r1, r2)
#     #print(dist)
#     hashKemeny6[[name]] <- dist
#   })
# }))
# 
getHash <- function(r1, r2 = NULL) {
  
  if(is.null(r2)) {
    paste0(paste(as.numeric(r1), collapse = ""))
  }
  else {
    paste0(paste(as.numeric(r1), collapse = ""),
           paste(as.numeric(r2), collapse = ""))
  }
 
}
