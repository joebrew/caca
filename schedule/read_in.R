library(XML)
library(googleVis)
library(dplyr)
cleanGoogleTable <- function(dat, table=1, skip=0, ncols=NA, nrows=-1, header=TRUE, dropFirstCol=NA){
  if(!is.data.frame(dat)){
    dat <- dat[[table]]
  }
  if(is.na(dropFirstCol)) {
    firstCol <- na.omit(dat[[1]])
    if(all(firstCol == ".") || all(firstCol== as.character(seq_along(firstCol)))) {
      dat <- dat[, -1]
    }
  } else if(dropFirstCol) {
    dat <- dat[, -1]
  }
  if(skip > 0){
    dat <- dat[-seq_len(skip), ]
  }
  if(nrow(dat) == 1) return(dat)
  if(nrow(dat) >= 2){
    if(all(is.na(dat[2, ]))) dat <- dat[-2, ]
  }
  if(header && nrow(dat) > 1){
    header <- as.character(dat[1, ])
    names(dat) <- header
    dat <- dat[-1, ]
  }
  # Keep only desired columns
  if(!is.na(ncols)){
    ncols <- min(ncols, ncol(dat))
    dat <- dat[, seq_len(ncols)]
  }
  # Keep only desired rows
  if(nrows > 0){
    nrows <- min(nrows, nrow(dat))
    dat <- dat[seq_len(nrows), ]
  }
  # Rename rows
  rownames(dat) <- seq_len(nrow(dat))
  dat
}


readGoogleSheet <- function(url, na.string="", header=TRUE){
  stopifnot(require(XML))
  # Suppress warnings because Google docs seems to have incomplete final line
  suppressWarnings({
    doc <- paste(readLines(url), collapse=" ")
  })
  if(nchar(doc) == 0) stop("No content found")
  htmlTable <- gsub("^.*?(<table.*</table).*$", "\\1>", doc)
  ret <- readHTMLTable(htmlTable, header=header, stringsAsFactors=FALSE, as.data.frame=TRUE)
  lapply(ret, function(x){ x[ x == na.string] <- NA; x})
}

link <- 'https://docs.google.com/spreadsheets/d/1S5qxi6EKlcZNzjBmUr6MqSb6_HYs6oWRNxbxU0RhYV4/pubhtml'

gdoc <- link
elem <- readGoogleSheet(gdoc)
df <- cleanGoogleTable(elem, table=1)
head(df)

#####
# CLEAN
#####
#df$Type <- factor(df$Type)
df$date <- as.Date(paste0(sub(' .*$', '', df$Time), '/2015'), 
                   format = '%m/%d/%Y')
df$time <- sub('(.*) ', '', df$Time)
temp <- strsplit(df$time, ':')
df$hour <- unlist(lapply(temp, function(x){as.numeric(x[[1]])}))
df$minute <- unlist(lapply(temp, function(x){as.numeric(x[[2]])}))
df$time2 <- df$hour + (df$minute / 60)

# Get day number
df$day <- as.numeric(df$date) - 16583
# df$day <- ifelse(df$hour <= 6,
#                  df$day -1,
#                  df$day)

# day-time
df$time3 <- (df$day *24) + df$time2

df$Duration <- as.numeric(df$Duration)
df$Duration[which(is.na(df$Duration))] <- 0

df$day_char <- as.character(df$day)

#####
# ORDER
#####
df <- df %>%
  arrange(date, hour, minute)

#####
# MAKE A CLEANED TYPE
#####
df$type <- ifelse(df$Type == 'Left breast',
                  'Breastfeed',
                  ifelse(df$Type == 'Right breast',
                         'Breastfeed',
                         ifelse(df$Type == 'Feed from pump',
                                'Breastfeed',
                         df$Type)))


# Better time objects
df$start <- as.POSIXlt(paste0(df$date, ' ', 
                              df$time,
                              ':00 EDT'))
df$end <- df$start + (df$Duration * 60)

########
# PLOT
########

# z <- df
# x <- gvisTimeline(data = z,
#              rowlabel = 'Type',
#              barlabel = 'time',
#              start = 'start',
#              end = 'end',
#              options=list(#timeline="{groupByRowLabel:false}",
#                           backgroundColor='#ffd', 
#                           height=350,
#                           colors="['#cbb69d', '#603913', '#c69c6e']"))
