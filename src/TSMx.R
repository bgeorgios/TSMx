# import packages

library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
library(tidyr)
library(forcats)
library(Kendall)

options(warn = -1) # disable warnings

# read data (.csv file with "Year" and "Value" columns)

data <- read_csv("EVI.csv")

# prepare row/column names for output matrices

years <- data %>% pull("Year")
r.names <- years[-length(years)]
c.names <- years[-1]
years <- years[-length(years)]

# initialize output matrices

sign.matrix <-
  matrix(data = NA,
         nrow = length(years),
         ncol = length(years))
pval.matrix <-
  matrix(data = NA,
         nrow = length(years),
         ncol = length(years))
slope.matrix <-
  matrix(data = NA,
         nrow = length(years),
         ncol = length(years))

# function to return remaining years given a start year

getRemain <- function(start.year) {
  years <- data %>% pull("Year")
  start.ind <- which(data[["Year"]] == start.year) + 1
  remain <- years[start.ind:length(years)]
  return (remain)
}

# function to subset data for a start/end year combination

splitData <- function(end.year, start.year) {
  keep <-
    which(data[['Year']] >= start.year & data[['Year']] <= end.year)
  batch <- data[keep,]
  return(batch)
}

# function to fit linear regression and return slope direction

fitReg <- function(batch) {
  trend <- lm(Value ~ Year, data = batch)
  slope <- coefficients(trend)[[2]]
  return(sign(slope))
}

# function to fit linear regression and return slope magnitude

fitRegv2 <- function(batch) {
  trend <- lm(Value ~ Year, data = batch)
  slope <- coefficients(trend)[[2]]
  return(slope)
}

# function to implement Mann-Kendall (MK) trend test and return significance
# the test is implemented only for n>=8

getMann <- function(batch) {
  if (nrow(batch) >= 8) {
    mk <- MannKendall(batch[['Value']])
    pval <- mk[['sl']]
  } else {
    pval <- NA
  }
  return(pval)
}

# function to return slope direction for all combinations given a start year

getSign <- function(start.year) {
  remaining <- getRemain(start.year)
  combs <- lapply(remaining, splitData, start.year = start.year)
  signs <- lapply(combs, fitReg)
  return(signs)
}

# function to return MK significance for all combinations given a start year

getPval <- function(start.year) {
  remaining <- getRemain(start.year)
  combs <- lapply(remaining, splitData, start.year = start.year)
  pvals <- lapply(combs, getMann)
  return(pvals)
}

# function to return slope magnitude for all combinations given a start year

getMagn <- function(start.year) {
  remaining <- getRemain(start.year)
  combs <- lapply(remaining, splitData, start.year = start.year)
  magns <- lapply(combs, fitRegv2)
  return(magns)
}

# retrieve slope direction, MK significance, and slope magnitude

signs <- lapply(years, getSign)
pvals <- lapply(years, getPval)
magns <- lapply(years, getMagn)

# fill-in output matrices

dimension <- nrow(sign.matrix)

for (i in 1:dimension) {
  sign.matrix[i, i:dimension] <- unlist(signs[i])
  pval.matrix[i, i:dimension] <- unlist(pvals[i])
  slope.matrix[i, i:dimension] <- unlist(magns[i])
}

sign.matrix <- data.frame(sign.matrix)
pval.matrix <- data.frame(pval.matrix)
slope.matrix <- data.frame(slope.matrix)

# rename rows/columns of output matrices

names(sign.matrix) <- c.names
row.names(sign.matrix) <- r.names
names(pval.matrix) <- c.names
row.names(pval.matrix) <- r.names
names(slope.matrix) <- c.names
row.names(slope.matrix) <- r.names

# pretty-print slope direction output matrix

sign.matrix <-
  sign.matrix %>% rownames_to_column() %>% gather(colname, value, -rowname)
names(sign.matrix) <- c("Start", "End", "Sign")
sign.matrix[['Color']] <- NA
sign.matrix [['Trend']] <- NA
pos.ind <- which(sign.matrix[['Sign']] > 0)
neg.ind <- which(sign.matrix[['Sign']] < 0)
sign.matrix[['Color']][pos.ind] <- "#99CCFF"
sign.matrix[['Color']][neg.ind] <- "#FF9966"
sign.matrix[['Trend']][pos.ind] <- "+"
sign.matrix[['Trend']][neg.ind] <- "-"
sign.matrix[['Trend']][is.na(sign.matrix[['Trend']])] <- ""

ggplot(sign.matrix,
       aes(x = End,
           fct_rev(Start),
           col = Color,)) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_fixed() +
  geom_tile(aes(fill = Color), color = 'black') + xlab("end year") + ylab("start year") +
  labs(title = "Slope direction", caption = "Boumis and Peter (2021)") +
  theme(
    axis.text.x = element_text(angle = 15),
    axis.text.y = element_text(angle = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "italic"),
    plot.caption = element_text(face = "italic")
  ) + scale_x_discrete(position = "top") +
  geom_text(aes(label = Trend), color = "black")

# save .png of slope direction output matrix

ggsave(
  "Slope-Direction.png",
  device = "png",
  unit = "in",
  height = 7,
  width = 12,
  dpi = 300
)

# pretty-print MK significance output matrix

pval.matrix <-
  pval.matrix %>% rownames_to_column() %>% gather(colname, value, -rowname)
names(pval.matrix) <- c("Start", "End", "Pval")
pval.matrix[['Color']] <- NA
pval.matrix [['Sign']] <- NA
sign.ind <- which(pval.matrix[['Pval']] < 0.05)
nsign.ind <- which(pval.matrix[['Pval']] > 0.05)
pval.matrix[['Color']][sign.ind] <- "#99FFFF"
pval.matrix[['Color']][nsign.ind] <- "#CC33CC"
pval.matrix[['Sign']][sign.ind] <- "<0.05"
pval.matrix[['Sign']][nsign.ind] <- ">0.05"
pval.matrix[['Sign']][is.na(pval.matrix[['Pval']])] <- ""

ggplot(pval.matrix,
       aes(x = End,
           fct_rev(Start),
           col = Color,)) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_fixed() +
  geom_tile(aes(fill = Color), color = "black") + xlab("end year") + ylab("start year") +
  labs(title = "Mann-Kendall trend significance", caption = "Boumis and Peter (2021)") +
  theme(
    axis.text.x = element_text(angle = 15),
    axis.text.y = element_text(angle = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "italic"),
    plot.caption = element_text(face = "italic")
  ) + scale_x_discrete(position = "top") +
  geom_text(aes(label = Sign), color = "black", size = 2)

# save .png of MK significance output matrix

ggsave(
  "MK-Significance.png",
  device = "png",
  unit = "in",
  height = 7,
  width = 12,
  dpi = 300
)

# pretty-print slope magnitude output matrix

slope.matrix <-
  slope.matrix %>% rownames_to_column() %>% gather(colname, value, -rowname)
names(slope.matrix) <- c("Start", "End", "Magnitude")

ggplot(slope.matrix, aes(End, fct_rev(Start), fill = Magnitude)) +
  xlab("end year") + ylab("start year") +
  geom_tile(color = "black") +
  coord_fixed() +
  labs(title = "Slope magnitude", caption = "Boumis and Peter (2021)") +
  scale_fill_gradient2(
    low = "#FF3300",
    mid = "#FFFFFF",
    high = "#0033FF",
    midpoint = 0.0
  ) +
  theme(
    axis.text.x = element_text(angle = 15),
    axis.text.y = element_text(angle = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "italic"),
    plot.caption = element_text(face = "italic")
  ) + scale_x_discrete(position = "top") +
  guides(fill = guide_colourbar(
    title = "",
    barwidth = 0.5,
    barheight = 7.5
  ))

# save .png of slope magnitude output matrix

ggsave(
  "Slope-Magnitude.png",
  device = "png",
  unit = "in",
  height = 7,
  width = 12,
  dpi = 300
)
