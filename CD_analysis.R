library("xlsx")


#Creates a blank data frame with 16 rows for later results storage
output2 <- data.frame(seq(1, 16, by = 1))

#Function for analysis of CD results
cd_results_analysis <- function(folder, file){


      #Imports CD results
      cd_res <- read.csv(paste0("Z:/users/wxn264/DIA_DDA_Nov_2017/Compound_Discoverer/HILIC_POS/", folder, "/HILIC_Plasma_POS_DDA_70K_", file, ".csv"))

      #Gets the number of compounds, number of compounds with no MS2, compounds with MS2 for the preferred ion, compounds with MS2 for another ion, compounds with no MS2
      best_match <- c((length(cd_res$Name)),
                     ((length(cd_res$Name)) - (length(subset(cd_res, cd_res$MS2 == "No MS2")[,1]))),
                      (length(subset(cd_res, cd_res$MS2 == "ddMS2 for preferred ion")[,1])), 
                      (length(subset(cd_res, cd_res$MS2 == "ddMS2 for other ion")[,1])),
                      (length(subset(cd_res, cd_res$MS2 == "No MS2")[,1])))

      #Repeats just to keep columns the same length
      best_match_pi <- c((length(cd_res$Name)),
                        ((length(cd_res$Name)) - (length(subset(cd_res, cd_res$MS2 == "No MS2")[,1]))),
                         (length(subset(cd_res, cd_res$MS2 == "ddMS2 for preferred ion")[,1])), 
                         (length(subset(cd_res, cd_res$MS2 == "ddMS2 for other ion")[,1])),
                         (length(subset(cd_res, cd_res$MS2 == "No MS2")[,1])))


         #Gets the number of compounds with a best match of 0 or more, then 10 or more etc up to 100. Repeated with the preferred ion
         for(s in seq(0, 100, by = 10)){

         best_match <- c(best_match, length(subset(cd_res, cd_res$mzVault.Best.Match >= s)[,1]))

         best_match_pi <- c(best_match_pi, length((subset(cd_res, cd_res$mzVault.Best.Match >= s & cd_res$MS2 == "ddMS2 for preferred ion"))[,1]))

         }

    
    #Prepares the reuslts for addition to the final output
    output <<- cbind(best_match, best_match_pi)
    colnames(output) <<- c((paste0(folder, "_best_match")), (paste0(folder, "_best_match_pi")))

    output2 <<- cbind(output2, output)

    rownames(output2) <<- c("Compounds",
                            "MS_MS",
                            "Preferred_Ion",
                            "Other_Ion",
                            "No_MS_MS",
                            ">=0",
                            ">=10",
                            ">=20",
                            ">=30",
                            ">=40",
                            ">=50",
                            ">=60",
                            ">=70",
                            ">=80",
                            ">=90",
                            ">=100")

    assign(x = "output3", value = output2, envir = .GlobalEnv)

    }

cd_results_analysis("Tra", "Tra_1")
cd_results_analysis("Tra_exc", "Tra_exc_1")
cd_results_analysis("Tra_inc", "Tra_inc_1")
cd_results_analysis("Tra_p_exc_1", "Tra_p_exc_1")
cd_results_analysis("Tra_p_exc_2", "Tra_p_exc_1_2")
cd_results_analysis("Mull1", "Mull1_1")
cd_results_analysis("Mull1_1", "Mull1_exc_1_1")
cd_results_analysis("Mull1_2", "Mull1_exc_1_2")
cd_results_analysis("Mull2", "Mull2_1")
cd_results_analysis("Mull2_1", "Mull2_exc_1_1")
cd_results_analysis("Mull2_2", "Mull2_exc_1_2")
cd_results_analysis("Mull3", "Mull3_1")
cd_results_analysis("Mull3_1", "Mull3_exc_1_1")
cd_results_analysis("Mull3_2", "Mull3_1_2")

write.csv(output3, "Z:/users/wxn264/DIA_DDA_Nov_2017/Results_powerpoints/HILIC_POS_CD.csv")

