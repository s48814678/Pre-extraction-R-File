
#Import publicly-available HAB dataset and requesting site summary.
HAB <- HarmfulAlgalBloom_MonitoringSites_251011
MonitoringSites <- MonitoringSites_Summary_10102025

#Add consistent date formatting
HAB$date_string<- as.character(format(as.Date(HAB$Date_Sample_Collected, "%d/%m/%Y"),"%Y-%m-%d"))
             
#Filter out non-Karenia sp. results. Some early water samples included total phytoplankton or other species.
HAB_Karenia = HAB[which(HAB$Result_Name=="Karenia sp."),]

#Join dataframes by unique SiteName variable.
names(HAB_Karenia)[2] <- "SiteName"
HAB_combined <- merge(HAB_Karenia,MonitoringSites, by="SiteName", all.x = TRUE)

#Remove sites with no location values for consistency.
HAB_combined <- HAB_combined[!is.na(HAB_combined$Latitude), ]

write.csv(HAB_combined, "HAB_combined.csv")