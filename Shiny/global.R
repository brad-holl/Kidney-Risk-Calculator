library(dplyr)
library(tidyverse)
library(limma)
library(reshape2)
library(DT)
library(stringr)
library(preprocessCore)
library(sva)
library(cvTools)      # Cross-validation
library(e1071)        # Support vector machines
library(randomForest) # Random forests
library(viridis)      # Viridis colour scheme
library(plotly)       # Interactive plots

load("merged_dataA.RData", verbose = TRUE)
load("merged_dataP.RData", verbose = TRUE)
load("OutcomeA.RData", verbose = TRUE)
load("OutcomeP.RData", verbose = TRUE)


# MODEL

# Adult model
X_gse1c <- t(merged_dataA)
y_gse1c <- OutcomeA
svm_fit <- svm(x = X_gse1c, y = as.factor(y_gse1c), probability = TRUE)

# Pediatric model
X_gse2c <- t(merged_dataP)
y_gse2c <- OutcomeP
svm_fit_P <- svm(x = X_gse2c, y = as.factor(y_gse2c), probability = TRUE)

#########################################################################

# raw DB
db_adult <- as.data.frame(merged_dataA)
db_pediatric <- as.data.frame(merged_dataP)

# Cleaned
# adult
patient_data_A <- t(db_adult)
prediction_A <- predict(svm_fit, newdata = as.data.frame(patient_data_A), probability = TRUE)

# get outcome
outcome_A <- as.data.frame(prediction_A) 
# get risk score
score_A <- as.data.frame(attr(prediction_A,"probabilities")) 
# combine them
databaseA <- subset(cbind(outcome_A, round(score_A*100, 2)), select = -Stable) 
# add age
databaseA$Category <- "Adult" 
# change rowname to id
databaseA$patientid <- rownames(databaseA) 
rownames(databaseA) <- NULL

# Rename the column names
colnames(databaseA)[1:4] <- c("outcome", "risk_score", "age_category", "patient_id")
# Reorder the columns
databaseA <- databaseA[, c("patient_id", "age_category", "outcome", "risk_score")]

databaseA
#write.csv(databaseA, "databaseA.csv", row.names = FALSE)

# pediatric
patient_data_P <- t(db_pediatric)
prediction_P <- predict(svm_fit_P, newdata = as.data.frame(patient_data_P), probability = TRUE)

# get outcome
outcome_P <- as.data.frame(prediction_P) 
# get risk score
score_P <- as.data.frame(attr(prediction_P,"probabilities")) 
# combine them
databaseP <- subset(cbind(outcome_P, round(score_P*100, 2)), select = -Stable) 
# add age
databaseP$Category <- "Pediatric" 
# change rowname to id
databaseP$patientid <- rownames(databaseP) 
rownames(databaseP) <- NULL

# Rename the column names
colnames(databaseP)[1:4] <- c("outcome", "risk_score", "age_category", "patient_id")
# Reorder the columns
databaseP <- databaseP[, c("patient_id", "age_category", "outcome", "risk_score")]

databaseP
#write.csv(databaseP, "databaseP.csv", row.names = FALSE)

# Combine all patients
database_all_patients <- rbind(databaseA, databaseP)

#########################################################################

# DASHBOARD - stable vs rejection sunburst graph
sunburst_df <- data.frame(labels=c("Stable", 
                                   "Rejected", 
                                   "Stable Pediatric", 
                                   "Stable Adult", 
                                   "Rejected Pediatric", 
                                   "Rejected Adult"),
                          values=c(sum(database_all_patients$outcome=="Stable"), 
                                   sum(database_all_patients$outcome=="Rejection"), 
                                   sum(databaseP$outcome=="Stable"), 
                                   sum(databaseA$outcome=="Stable"), 
                                   sum(databaseP$outcome=="Rejection"), 
                                   sum(databaseA$outcome=="Rejection")),
                          parents=c(NA, 
                                    NA, 
                                    "Stable", 
                                    "Stable", 
                                    "Rejected", 
                                    "Rejected")
)

#########################################################################

# ANALYSIS

traindataA <- as.data.frame(merged_dataA)
designA <- model.matrix(~ OutcomeA, data = traindataA)
fitA <- lmFit(traindataA, designA)
fitA <- eBayes(fitA)

topTableA <- suppressMessages(topTable(fitA, genelist = traindataA$gene, n = 5))
top50TableA <- suppressMessages(topTable(fitA, genelist = traindataA$gene, n = 50))

genesA <- rownames(topTableA)[1:5]
subset_dataA <- merged_dataA[genesA, ]
dataA <- reshape2::melt(subset_dataA, varnames = c("Gene", "Sample"), value.name = "Expression")
dataA

topA <- rownames(topTableA)
topTable50_A <- rownames(suppressMessages(topTable(fitA, genelist = traindataA$gene, n = 50)))

traindataP <- as.data.frame(merged_dataP)
designP <- model.matrix(~ OutcomeP, data = traindataP)
fitP <- lmFit(traindataP, designP)
fitP <- eBayes(fitP)

topTableP <- suppressMessages(topTable(fitP, genelist = traindataP$gene, n = 5))
top50TableP <- suppressMessages(topTable(fitP, genelist = traindataP$gene, n = 50))

genesP <- rownames(topTableP)[1:5]
subset_dataP <- merged_dataP[genesP, ]
dataP <- reshape2::melt(subset_dataP, varnames = c("Gene", "Sample"), value.name = "Expression")

topP <- rownames(topTableP)

topTable50_P <- rownames(suppressMessages(topTable(fitP, genelist = traindataP$gene, n = 50)))


#subsetting data to top 5 genes
topTable_pediatric <- db_pediatric[topP, ]
topTable_adult<- db_adult[topP, ]

# subsetting data to top 50
top50_P <-db_pediatric[topTable50_P, ]
top50_A<- db_adult[topTable50_A, ]


topTable_pediatric$median <- apply(topTable_pediatric, 1, median, na.rm=TRUE)
topTable_adult$median <- apply(topTable_adult, 1, median, na.rm=TRUE)

top50_P$median <- apply(top50_P, 1, median, na.rm=TRUE)
top50_A$median <- apply(top50_A, 1, median, na.rm=TRUE)

adult_sta <- db_adult[databaseA$outcome=="Stable",]
adult_sta <- adult_sta[topTable50_A,]
adult_sta$median <- apply(adult_sta, 1, median, na.rm=TRUE) 

adult_ar <- db_adult[databaseA$outcome=="Rejection",]
adult_ar <- adult_ar[topTable50_A,]
adult_ar$median <- apply(adult_ar, 1, median, na.rm=TRUE)

ped_sta <- db_pediatric[databaseP$outcome=="Stable",]
ped_sta <- ped_sta[topTable50_P,]
ped_sta$median <- apply(ped_sta, 1, median, na.rm=TRUE)

ped_ar <- db_pediatric[databaseP$outcome=="Rejection",]
ped_ar <- ped_ar[topTable50_P,]
ped_ar$median <- apply(ped_ar, 1, median, na.rm=TRUE)

col_names <- c("gene_symbol", "sta", "ar")

top_genes_A <- data.frame(
  gene_symbol <- rownames(top50_A),
  stable <- adult_sta$median,
  rejection <- adult_ar$median
)
colnames(top_genes_A) <- col_names

top_genes_P <- data.frame(
  gene_symbol <- rownames(top50_P),
  stable <- ped_sta$median,
  rejection <- ped_ar$median
)
colnames(top_genes_P) <- col_names

#Adult
genesA <- rownames(topTableA)[1:5]
subset_dataA <- merged_dataA[genesA, ]

dataA <- reshape2::melt(subset_dataA, varnames = c("Gene", "Sample"), value.name = "Expression")

# Boxplot
# ggplot(dataA, aes(x = Gene, y = Expression)) +
#   geom_boxplot() +
#   labs(title = "Boxplot - Top 5 Genes in Aediatric Dataset",
#        x = "Genes",
#        y = "Expression") +
#   theme_minimal()

#Pediatric
genesP <- rownames(topTableP)[1:5]
subset_dataP <- merged_dataP[genesP, ]

dataP <- reshape2::melt(subset_dataP, varnames = c("Gene", "Sample"), value.name = "Expression")

# Boxplot
# ggplot(dataP, aes(x = Gene, y = Expression)) +
#   geom_boxplot() +
#   labs(title = "Boxplot - Top 5 Genes in Pediatric Dataset",
#        x = "Genes",
#        y = "Expression") +
#   theme_minimal()
