#### Function to settle disputes more dramatically than a coin toss: 
# A million votes are taken one at a time, and results are plotted on the graphics device in real-time. 

putItToAVote = function(candidates = c("pizza", "burritos"), 
                        n.votes = 1e5, 
                        report.every = 1000, 
                        customcols = NULL) {
  ### Arguments:
  # candidates: character vector of candidates to be voted on
  # n.votes: how many voters
  # report.every: replot the results every time this many new votes have been taken. Controls the speed of the animation. 
  # customcols: optional, a vector of color names
  
  # custom colors:
  if (length(customcols) == 0) {
    customcols = c("darkblue", "orange", "forestgreen", "red", "grey20", "gold", "chartreuse3", "slateblue3", "firebrick", "steelblue3")
  }
  customcols = c(customcols, setdiff(sample(colors()), customcols))
  customcols = customcols[1:length(candidates)]
  names(customcols) = candidates
  
  # Cast votes one at a time
  votes = c()
  for (i in 1:n.votes){
    votes = c(votes, sample(candidates, 1))
    totals = c()
    # update plot every 1000 votes (or whatever report.every specifies):
    if((i %% report.every == 0) & (i > 1)){
      # get cumulative votes for each
      totals = matrix(NA, i, length(candidates))
      colnames(totals) = candidates
      for (cand in candidates) {
        totals[, cand] = cumsum(votes == cand)
      }

      # subsample for plotting speed:
      increment = round(seq(1, i, length.out = min(c(i, report.every))))
      
      # plot for two candidates:
      if (length(candidates) == 2) {
        plot(0, col = 0, xlim = c(1, i), ylim = c(range(c(0, totals[, 1] - totals[, 2]))), 
             ylab = paste0(candidates[2], " <--- ---> ", candidates[1]), cex = 0.2,
             xlab = "# votes", cex.lab = 1.5)
        lines((1:i)[increment], totals[increment, 1] - totals[increment, 2],
              col = customcols[1 + (totals[i, 2] > totals[i, 1])])
        abline(h = 0)
        # running tally
        legend("top", legend = paste0(table(votes), " ", names(table(votes))))
      }
     
      
      # plot for >2 candidates:
      deltas = sweep(totals, 1, rowMeans(totals), "-")
      if (length(candidates) > 2) {
        plot(0, col = 0, xlim = c(1, i), ylim = range(c(0, deltas)), 
             ylab = "Votes vs. an even split", cex = 0.2,
             xlab = "# votes", cex.lab = 1.5)
        for (cand in candidates) {
          lines((1:i)[increment], deltas[increment, cand],
                col = customcols[cand])
        }
        abline(h = 0, col = "grey50")
        legend("top", lty = 1, col = customcols, 
               legend = paste0(names(customcols), ": ", totals[i, names(customcols)] ), cex = 1.2)
      }
    }
  }
  # announce results
  legend("center", cex = 1.5,
         legend = paste0("And the winner is... ", 
                         names(table(votes))[order(table(votes), decreasing = T)[1]]))
}

