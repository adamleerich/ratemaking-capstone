# Author: Adam L. Rich
# Date:   August 21, 2017
# Description:
#
#   Functions for dealing with dates in YYYYMM format
#


year_yyyymm <- function(d) {
  d <- as.integer(d)
  floor(d / 100)
}

month_yyyymm <- function(d) {
  d <- as.integer(d)
  d %% 100
}

add_yyyymm <- function(d, m) {
  m <- as.integer(m)
  dy <- year_yyyymm(d)
  dm <- month_yyyymm(d)
  am <- dm + m
  ay <- ifelse(am == 0, 0, floor((am - 1) / 12))
  ry <- dy + ay
  rm <- am - ay * 12
  rd <- ry * 100 + rm
  return(rd)
}

diff_yyyymm <- function(d1, d2) {
  yd1 <- year_yyyymm(d1)
  yd2 <- year_yyyymm(d2)
  md1 <- month_yyyymm(d1)
  md2 <- month_yyyymm(d2)
  md2 - md1 + (yd2 - yd1) * 12
}



# It doesn't matter if the date is character or numeric
year_yyyymm('201706')
year_yyyymm(201706)
month_yyyymm('201706')
month_yyyymm(201706)



# Increments year when necessary
add_yyyymm('201006', 12)
add_yyyymm('201006', 31)



# Handles negatives, too
add_yyyymm('201006', -12)
add_yyyymm('201006', -31)



# Diff of dates returns month (second - first)
diff_yyyymm('201706', 201710)
diff_yyyymm(201612, 201611)




# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

# Paste data into R
paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}


copy.model <- function(m) {
  d <- data.frame(
    variable = names(m$coefficients),
    coefficient = m$coefficients
  )
  copy.table(d)
  d
}
