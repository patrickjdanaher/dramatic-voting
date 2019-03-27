# simple demonstration of dramatic voting:
source("dramatic voting.R")

# Demo with two candidates:
putItToAVote(candidates = c("pizza", "burritos"), n.votes = 1e5, report.every = 1000, customcols = NULL)

# Demo with 3 candidates:
putItToAVote(candidates = c("jogging", "happy hour", "work late"), n.votes = 1e5, report.every = 1000, customcols = NULL)