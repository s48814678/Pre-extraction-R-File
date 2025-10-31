install.packages("caret")
library(caret)


#Check for total number of samples included
samples_total<- unique(results_extract$ReferenceID)

#Decode Level 2 Flags from https://oceancolor.gsfc.nasa.gov/resources/atbd/ocl2flags/?utm_source=chatgpt.com
l2_flags_bits <- c(
  "ATMFAIL" = 0,
  "LAND" = 1,
  "PRODWARN" = 2,
  "HIGLINT" = 3,
  "HILT" = 4,
  "HISATZEN" = 5,
  "COASTZ" = 6,
  "STRAYLIGHT" = 8,
  "CLDICE" = 9,
  "HISOLZEN" = 12,
  "LOWLW" = 14,
  "CHLFAIL" = 15,
  "NAVWARN" = 16,
  "ABSAER" = 17,
  "MAXAERITER" = 19,
  "MODGLINT" = 20,
  "CHLWARN" = 21,
  "ATMWARN" = 22,
  "SEAICE" = 24,
  "NAVFAIL" = 25,
  "FILTER" = 26,
  "BOWTIEDEL" = 28,
  "HIPOL" = 29,
  "PRODFAIL" = 30
)

add_l2_flag_columns <- function(df, flag_column = "l2_flags", flag_bits = l2_flags_bits) {
  for (flag_name in names(flag_bits)) {
    bit_pos <- flag_bits[flag_name]
    # Create bitmask (2^bit position)
    mask <- bitwShiftL(1L, bit_pos)
    # Use bitwise AND to check if bit is set
    df[[flag_name]] <- ifelse(bitwAnd(df[[flag_column]], mask) != 0, 1, 0)
  }
  return(df)
}

results_extract <- add_l2_flag_columns((results_extract))

#Check number of CLDICE masks
samples_CLDICE <- results_extract[results_extract$CLDICE == 1, ]


#Remove null aph rows
results_filtered <- results_extract[which(!is.na(results_extract$aph_400)),]
samples_1<- unique(results_filtered$ReferenceID)

#Remove no result rows
results_filtered2 <- results_filtered[which(!is.na(results_filtered$Result_Value_Numeric)),]
samples_2<- unique(results_filtered2$ReferenceID)

#Remove non-negative depth rows
results_filtered3 <- results_filtered2[which(results_filtered2$Depth<0),]
samples_3<- unique(results_filtered3$ReferenceID)

#Apply LAND mask
results_filtered4 <- results_filtered3[which(results_filtered3$LAND==0),]
samples_4<- unique(results_filtered4$ReferenceID)

#Remove null BGC rows
results_filtered5 <- results_filtered4[
  complete.cases(results_filtered4[, c("chlor_a", "carbon_phyto", "poc")]),
]
samples_5<- unique(results_filtered5$ReferenceID)

#Remove invalid aph values
results_filtered6 <- results_filtered5[which(results_filtered5$aph_400>0),]
samples_6<- unique(results_filtered6$ReferenceID)

#Check remaining number of samples
nrow(unique(results_filtered6[, c("granule_id", "ReferenceID")]))

#Recode into 5 classes
results_filtered6$class[results_filtered6$Result_Value_Numeric <1000] <- 1
results_filtered6$class[results_filtered6$Result_Value_Numeric >=1000 & results_filtered6$Result_Value_Numeric <10000] <- 2
results_filtered6$class[results_filtered6$Result_Value_Numeric >=10000 & results_filtered6$Result_Value_Numeric <50000] <- 3
results_filtered6$class[results_filtered6$Result_Value_Numeric >=50000 & results_filtered6$Result_Value_Numeric <100000] <- 4
results_filtered6$class[results_filtered6$Result_Value_Numeric >=100000] <- 5

#Uncomment for binary classification
#results_filtered6$class <- 2
#results_filtered6$class[results_filtered6$Result_Value_Numeric <10000] <- 1

training <- results_filtered6[,c(59,1:18,20:22,24,34:58)]

#Uncomment for reduced Model
#training <- results_filtered6[,c(59,24,22,20,34,21,18,5,6,7,4)]

#Uncomment for Chlor-a Model
#training <- results_filtered6[,c(59,20,24,34:58)]

#Uncomment for Chlor-a Reduced Moded
#training <- results_filtered6[,c(59,20,24,34,37,52)]

#Iterated partition n=20. Change file number each time. Returns min and max
# classes so binary can be checked.
#Stratified partition due to uneven class distribution.

for (i in 1:20) {
  set.seed(i)
  
  # Partition the data
  train_index <- createDataPartition(training$class, p = 0.7, list = FALSE)
  training_df <- training[train_index, ]
  validation_df <- training[-train_index, ]
  
  
  # Check class distribution
  print(summary(training_df$class))
  print(summary(validation_df$class))
  
  # Save the files. Uncomment for combined dataset.
  #write.csv(training, paste0("results_filtered_1_", i, ".csv"), row.names = FALSE)
  write.csv(training_df, paste0("training_df_1_", i, ".csv"), row.names = FALSE)
  write.csv(validation_df, paste0("validation_df_1_", i, ".csv"), row.names = FALSE)
}














