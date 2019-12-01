library(stringr)

# JG : I made this for module 5.
#      The live session work.


pp_cor = function(a) {
  result = round(cor(a) * 100)
  for(row in 1:length(result[1,])) {
    for(col in 1:length(result[1,])) {
      if(col < row) {
        result[row, col] = '.'
      } else {
        result[row, col] = str_pad(result[row, col], 3, pad=" ")
      }

    }
  }
  class(result) = "noquote"
  result
}
