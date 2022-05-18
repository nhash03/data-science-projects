library(tidyverse)
library(readr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(rotations)
library(mosaic)
# open the main data
velocities <- read.csv("Downloads/velocities_final 2.csv", header = FALSE)
new_file <- pivot_longer(velocities, cols = V1:V900, names_to = "data", 
                         values_to = "val")
vel <- new_file$val

#make a histogram from velocities
hist <- hist(vel,
             main = "histogram of tokhms",
             xlab= "velocities",
             col="pink",
             breaks = 20, freq = FALSE, pred= TRUE)
text(hist$mids, hist$density, labels = hist$counts, adj = c(0.1,0.8))

# find the perfect maxwell-boltzmann fit
library(SciViews)
bins <- read.csv("Downloads/data_final.csv")

bins <- mutate(bins, lnV = ln(v))
bins <- mutate(bins, lnF = ln(freq/900))
f2 <- fitModel(
  lnF ~ -0.2257913526 - 3*ln(A) + 2*lnV - ((v^2)/(2*A^2)),
  data = bins)
A <- coef(f2)

#add the best fit to the velocities
curve(0.7978845608*(A^-3)*(x^2)
      *exp(-x^2/(2*(A^2))), add= TRUE)