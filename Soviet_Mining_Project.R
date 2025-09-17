schedule = read_csv("C:/Users/ksnow2024/OneDrive - amsacs.org/Documents/DataSci/csvFiles/schedule.csv")
productivity = read_csv("C:/Users/ksnow2024/OneDrive - amsacs.org/Documents/DataSci/csvFiles/productivity.csv")
productivity = productivity %>% pivot_longer(cols = c("8h-CopperQuota", "9h-CopperQuota", "10h-CopperQuota", "11h-CopperQuota", 
                                                      "12h-CopperQuota", "13h-CopperQuota", "14h-CopperQuota", "15h-CopperQuota", 
                                                      "16h-CopperQuota", "17h-CopperQuota", "18h-CopperQuota", "19h-CopperQuota", 
                                                      "20h-CopperQuota", "8h-CopperProduced", "9h-CopperProduced", "10h-CopperProduced", 
                                                      "11h-CopperProduced", "12h-CopperProduced", "13h-CopperProduced", "14h-CopperProduced", 
                                                      "15h-CopperProduced", "16h-CopperProduced", "17h-CopperProduced", "18h-CopperProduced", 
                                                      "19h-CopperProduced", "20h-CopperProduced"), names_to = "Times", values_to = "Production")
productivity = separate(productivity, Times, into = c("Hour", "Copper"), sep = "h-")
# pivot_wider(select(productivity, c(Copper, Production)), names_from = Copper, values_from = Production)