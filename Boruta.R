# Example
# Feature selection under Boruta
library(TH.data)
library(Boruta)
library(caret)
library(foreign)
library(mice)
library(skimr)

data("GlaucomaM", package = "TH.data")
trainData <- GlaucomaM
head(trainData)

trainData$Class


output <- Boruta(Class ~ ., data = na.omit(trainData), doTrace = 0)
names(output)

# Significant variables including tentatives 
output_sig <- getSelectedAttributes(output, withTentative = TRUE)
output_sig

plot(output, cex.axis = .7, las = 2)

############################################################################

setwd("/youth main")


w3 <- read.spss("w3Reduced1.sav", to.data.frame = TRUE, use.value.labels = TRUE, use.missings = TRUE)
w3$y3_doby <- as.numeric(levels(w3$y3_doby))[w3$y3_doby]
w3$y3_gradesc_ge <- as.numeric(levels(w3$y3_gradesc_ge))[w3$y3_gradesc_ge]
w3$y3_sat1 <- as.numeric(levels(w3$y3_sat1))[w3$y3_sat1]
w3$y3_attgr8 <- as.numeric(levels(w3$y3_attgr8))[w3$y3_attgr8]


data.impt <- mice(w3, maxit = 3, method = "pmm", seed = 123)
summary(data.impt)
w3.full <- complete(data.impt)

sum(is.na(w3.full))

skim(w3.full)
str(w3.full)

# Preparation for one-hot encoding
w3Muslims <- subset(w3.full, y3_rel1 == 'islam')

w3Muslims$y3_generationG <- NULL
w3Muslims$y3_countorig_geG <- NULL 
w3Muslims$y3_relb1 <- NULL
w3Muslims$y3_rel2 <- NULL
#w3Muslims$y3_relb2 <- NULL
w3Muslims$y3_heab2 <- NULL
w3Muslims$y3_heab4 <- NULL
w3Muslims$y3_heab6 <- NULL
w3Muslims$y3_tol1 <- NULL
w3Muslims$y3_tol2 <- NULL
w3Muslims$y3_tol3 <- NULL
w3Muslims$y3_tol4 <- NULL
w3Muslims$y3_del4 <- NULL
w3Muslims$y3_atrel1 <- NULL
w3Muslims$y3_atrel2 <- NULL
w3Muslims$y3_atrel3 <- NULL
w3Muslims$y3_atrel4 <- NULL

age <- 2013 - w3Muslims$y3_doby
w3Muslims <- data.frame(w3Muslims, age)
w3Muslims$y3_doby <- NULL
w3Muslims$y3_rel1 <- NULL
rel2 <- w3Muslims$y3_rel2

# w3Muslims$y3_rel2 <- NULL

output <- Boruta(y3_relb2 ~ ., data = w3Muslims, doTrace = 0)
names(output)


# Significant variables including tentatives 
output_sig <- getSelectedAttributes(output, withTentative = TRUE)
output_sig

plot(output, cex.axis = .7, las = 2)