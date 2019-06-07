
# https://www.r-bloggers.com/self-organising-maps-for-customer-segmentation-using-r/

# setwd()

library(readr)
library(readxl)
library(tidyverse)
library(kohonen)
library(here)
library(BBmisc)

source(here("src", "KohonenSOM.R"))
source(here("src", "ReadData.R"))

df <- ReadData()

# All colnames to upper cases
colnames(df) <- toupper(colnames(df))
# Remove all non latin characters from colnames
colnames(df) <- stringi::stri_trans_general(colnames(df), 'latin-ascii')

# Subset data if needed
df <- filter(df, LM == 4)
df <- select(df, K1A1:K3B)

# normalize your data to eliminate the effect of having different scales for different variables:
df <- BBmisc::normalize(df, method = "range", range = c(0, 1), margin = 2L, on.constant = "quiet")

# Fit the model:

# Aim for at least 5-10 samples per node when choosing map size:
samples <- 15 

rows <- dim(df)[1]
# map size: x^2
x <- round((sqrt(rows/samples)), 0)
y <- x

set.seed(4)
som_model <- FitKohonenSOM(df, x, y, 2000)

# has the model stabilized? 
plot(som_model, type="changes")
# If the model is not stable, do more iterations
# are there enough samples per node? 
plot(som_model, type="count")
# If counts per plot is too small/big (less than 5-10 samples per node) adjust
# samples variable accordingly
summary(som_model)

# If you change any of the mentioned parameters, rerun the fitting process!
# if you are happy with the model, plot the maps

setwd(here("output"))
PrintKohonenSOM(som_model, "Nice SOM maps")
