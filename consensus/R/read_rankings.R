read_rankings <- function(file_path, verbose = FALSE) {
  conn <- file(file_path,open="r")
  lines <- readLines(conn)
  the_rankings <- matrix(ncol = length(lines))
  for (line in lines){
    r <- parse_ranking(line)
    if(verbose)
      print(r)
    the_rankings <- the_rankings %>% rbind(r)
    print("---")
  }
  close(conn)
  the_rankings <- the_rankings[-1, ]
  rownames(the_rankings) <- NULL
  return(the_rankings)
}
