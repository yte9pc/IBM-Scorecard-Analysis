
# Pretty print the max/min results of leaps::regsubsets
pp_allreg = function(allreg) {
  best <- as.data.frame(summary(allreg)$outmat)
  best$p <- as.numeric(substr(rownames(best),1,1))+1
  best$r2 <- summary(allreg)$rsq
  best$adjr2 <- summary(allreg)$adjr2
  best$mse <- (summary(allreg)$rss)/(dim(ibm)[1]-best$p)
  best$cp <- summary(allreg)$cp
  best$bic <- summary(allreg)$bic
  best$which = summary(allreg)$which

  # Helper function
  # ----------------
  # given a row from summary(regsubsets(...))$which
  # return the ~ ... right hand side
  fmrhs = function(w) {
    cols = stringr::str_replace(colnames(w), "\\(Intercept\\)", "1")
    paste(cols[w[1,]], collapse = " + ")
  }


  criteria = c("r2", "adjr2", "mse", "cp", "bic")
  desc =      c(T,    T,       F,     F,     F)

  best_rownames = rownames(best)
  r = matrix(nrow = 0, ncol = 4)
  rn = c()
  for(i in 1:length(criteria)) {
    criterion = criteria[i]
    decreasing = desc[i]

    # index for best row using this criterion
    o = order(best[,criterion], decreasing = decreasing)[1]

    # grab the best row
    b = best[o,]

    # get the which data and criterion value
    w = b$which
    val = b[,criterion]

    # get a formula name
    fm = paste("(response) ~ ", fmrhs(w))

    # append data
    newdata = matrix(c(fm, criterion, val, decreasing), nrow = 1, ncol = 4)
    append(rn, best_rownames[o])
    r = rbind(r, newdata)
  }
  rownames(r) = rep(" ", nrow(r))
  colnames(r) = c("Regressors", "Criterion", "Criterion value", "Minimized")

  # print nicely
  for(j in 1:nrow(r)) {
    row = r[j,]
    regressors = row[1]
    criterion = row[2]
    value = row[3]
    decreasing = row[4]
    type_text = ifelse(decreasing, "Maximized: ", "Minimized: ")
    cat(paste(type_text, criterion, " with value ", value, sep = ""))
    cat("\n")
    cat(paste("   Formula: ", regressors, sep = ""))
    cat("\n")
    cat("\n")
  }

}
