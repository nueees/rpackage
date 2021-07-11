################purrr########################
#setwd("C:/Users/Administrator/Documents/R/Kaggle/rpackage/000_data")
# URL <- "http://static.lib.virginia.edu/statlab/materials/data/stocks.zip"
# download.file(url = URL, destfile = basename(URL))
# unzip(basename(URL))
# # get all files ending in csv
# files <- list.files(pattern = "csv$") 
# # read in data
# dat <- lapply(files, read.csv)
# names(dat) <- gsub("\\.csv", "", files) # remove file extension
# 
# dat2 <- map(files, read.csv)
# names(dat2) <- gsub("\\.csv", "", files)

setwd("C:/Users/Administrator/Documents/R/Kaggle/rpackage/002_purrr")

# 참고자료 : https://www.rebeccabarter.com/blog/2019-08-19_purrr/#map-functions-beyond-apply

library(purrr)

my_first_list <- list(my_number = 5,
                      my_vector = c("a", "b", "c"),
                      my_dataframe = data.frame(a = 1:3, b = c("q", "b", "z"), c = c("bananas", "are", "so very great")))
my_first_list


addTen <- function(.x) {
  return(.x + 10)
}
library(tidyverse)
map(.x = c(1, 4, 7), 
    .f = addTen)
map(list(1, 4, 7), addTen)

map(data.frame(a = 1, b = 4, c = 7), addTen)








