electionData = read_csv("C:/Users/ksnow2024/OneDrive - amsacs.org/Documents/DataSci/csvFiles/resultsAnglais.csv")
ageData = read_csv("C:/Users/ksnow2024/OneDrive - amsacs.org/Documents/DataSci/csvFiles/AgeData.csv")
householdData = read_csv("C:/Users/ksnow2024/OneDrive - amsacs.org/Documents/DataSci/csvFiles/HouseholdData.csv")
languageData = read_csv("C:/Users/ksnow2024/OneDrive - amsacs.org/Documents/DataSci/csvFiles/LanguageData.csv")
incomeData = read_csv("C:/Users/ksnow2024/OneDrive - amsacs.org/Documents/DataSci/csvFiles/IncomeData.csv")


#Question 1 Which riding in Quebec has the most registered voters? Which has 
#the fewest?
maxPop = summarize(electionData, maximum = max(RegisteredVoters))
electionData %>% filter_all(any_vars(. %in% c(maxPop))) %>%
  filter(VotesLead != 0) %>% #this filter is just so the pull only returns one value
  pull(10)
#Brome-Missisquoi 66769
minPop = summarize(electionData, minimum = min(RegisteredVoters))
electionData %>% filter_all(any_vars(. %in% c(minPop))) %>% 
  filter(VotesLead != 0) %>% #this filter is just so the pull only returns one value
  pull(10)
#Îles-de-la-Madeleine 11159

#Question 2 What riding had the highest voter turnout among registered voters?
maxTurnOut = electionData %>% 
  summarize(percentages = TotalVotes / RegisteredVoters * 100) %>% 
  summarize(maximum = max(percentages))
  electionData %>% mutate(percentages = TotalVotes / RegisteredVoters * 100) %>% 
  filter_all(any_vars(. %in% c(maxTurnOut))) %>% 
  filter(VotesLead != 0) %>% #this filter is just so the pull only returns one value
  pull(10)
#Louis-Hébert 81.08685%
#minTurnOut = electionData %>% 
  #summarize(percentages = TotalVotes / RegisteredVoters * 100) %>% 
  #summarize(minimum = min(percentages))
#electionData %>% 
  #mutate(percentages = TotalVotes / RegisteredVoters * 100) %>% 
  #filter_all(any_vars(. %in% c(minTurnOut))) %>% 
  #filter(VotesLead != 0) %>% #this filter is just so the pull only returns one value
  #pull(10)
#Ungava 30.21476%

#Question 3 How many seats did each party win? What was the breakdown of the 
#popular vote?
winningSeats = electionData %>% 
  filter(VotesLead != 0) %>% 
  group_by(PoliticalPartyAbbrev) %>% 
  summarize(Seats = n())
print(winningSeats)
votesPerParty = electionData %>% 
  group_by(PoliticalPartyAbbrev) %>% 
  summarize(votes = sum(Votes))
votesPerParty = votesPerParty %>% 
  mutate(percentage = votes / sum(votes) * 100)
print(votesPerParty)

#Question 4 Create a plot/plots for both the popular vote and seat totals. 
#Style points if you can convey both things in the same plot in a way that is 
#both visually pleasing and informative?
ggplot(winningSeats, aes(x = PoliticalPartyAbbrev, y = Seats, fill = PoliticalPartyAbbrev)) + geom_col() + 
  labs(x = "Political Party", title = "Seats Won by Each Political Party") +
  scale_fill_manual(values = c("#00a7e6", "#ed1c2e", "#005bab", "#ff5503"))
ggplot(votesPerParty, aes(PoliticalPartyAbbrev, y = percentage, fill = PoliticalPartyAbbrev)) + geom_col() +
  labs(x = "Political Party", y = "Percentage of Total Votes", title = "Breakdown of Popular Vote")
seatsWonAndPop = full_join(votesPerParty, winningSeats) %>% 
  replace_na(list(Seats = 0))
ggplot(seatsWonAndPop, aes(x = Seats, y = percentage, color = PoliticalPartyAbbrev)) + geom_point() +
  labs(x = "Seats won", y = "Percentage of Total Votes", title = "Seats Won V. Percentage of Total Votes")

#Question 5 Qu ́ebec Solidaire and the Parti Qu ́eb ́ecois are somewhat similar
#ideologically – so much so that there was a proposal for the parties to form 
#an alliance in the 2018 provincial election. Although this proposal failed, 
#it can be fun to speculate what might have been. Imagine that Qu ́ebec
#Solidaire and the Parti Qu ́eb ́ecois had run as a single party in the most
#recent election. Imagine that their single candidate in each riding received 
#as many votes as the sum of the two parties’ votes in the actual election that 
#took place. How many seats would this united party have won?
winningSeats2 = electionData %>%
  filter(VotesLead != 0 | PoliticalPartyAbbrev == "Q.S." | PoliticalPartyAbbrev == "P.Q.") %>%
  mutate(PoliticalPartyAbbrev = str_replace(PoliticalPartyAbbrev, "Q.S.", "United")) %>%
  mutate(PoliticalPartyAbbrev = str_replace(PoliticalPartyAbbrev, "P.Q.", "United")) %>%
  group_by(PoliticalPartyAbbrev, RidingName) %>%
  summarize(CombinedVotes = sum(Votes)) %>%
  group_by(RidingName) %>%
  filter(CombinedVotes == max(CombinedVotes)) %>%
  group_by(PoliticalPartyAbbrev) %>%
  summarize(Seats = n())
print(winningSeats2)
#19 seats

#Question 6 Now let’s move on to census data. What is the youngest riding,
#measured by mean age? What is the oldest?
ageData = ageData %>% 
  pivot_longer(cols = 2:127, names_to = "Riding", values_to = "Population")
lowestMeanAge = ageData %>% 
  filter(Constituency == "Mean Age", Riding != "Province") %>% 
  summarize(min = min(Population))
ageData %>% filter_all(any_vars(. %in% c(lowestMeanAge))) %>% 
  pull(2)
#Ungava 32.6
highestMeanAge = ageData %>% 
  filter(Constituency == "Mean Age", Riding != "Province") %>% 
  summarize(max = max(Population))
ageData %>% filter_all(any_vars(. %in% c(highestMeanAge))) %>% 
  pull(2)
#49.1 Gaspé and Îles-de-la-Madeleine

#Question 7 Make a plot comparing the proportion of households that are 
#single-family homes within a riding to the proportion of the population of 
#the riding speaking French at home. Do single-family homes seem to correlate 
#with a riding being more or less Francophone?
householdData = householdData %>% 
  transmute_all(funs(str_remove(., ",")))
householdData = householdData %>% 
  pivot_longer(cols = 3:128, names_to = "Riding", values_to = "Population")
singleFamilies = householdData %>% 
  filter(Constituency == "Households with a Single Family and No Additional People")
languageData = languageData %>%
  pivot_longer(cols = 3:128, names_to = "Riding", values_to = "Population")
frenchAtHome = languageData %>%
  filter(Category == "Language Spoken At Home", Constituency == "French")
singleFamiliesFrench = full_join(singleFamilies, frenchAtHome, by = "Riding") %>%
  filter(Riding != "Province") %>%
  mutate(Population.x = as.numeric(Population.x))
ridingPopulation = ageData %>% 
  filter(Constituency == "Total Population") %>%
  pivot_longer(cols = 2:127, names_to = "Riding", values_to = "Population") %>%
  filter(Riding != "Province")
singleFamiliesFrench = full_join(singleFamiliesFrench, ridingPopulation) %>%
  mutate(percentageSingleFamily = Population.x / Population * 100) %>%
  mutate(percentageFrench = Population.y / Population * 100)
ggplot(singleFamiliesFrench, aes(x = percentageSingleFamily, y = percentageFrench)) + geom_point(color = "Blue") + 
  labs(x = "Single Family Population", y = "French Speaking Homes Population", title = "Single Families v French Speaking Homes")
#Yes, single-family homes seem to be more likely to be a French speaking home.

#Question 8 Make a plot showing the distribution of income among men and women, 
#side by side in any district. Mention the district name in the plot.
#I picked the riding, Groulx
incomeData = incomeData %>% 
  mutate(Province = str_remove_all(Province, ",")) %>%
  mutate(Province = as.numeric(Province)) %>%
  pivot_longer(cols =3:128, names_to = "Riding", values_to = "Population") 
incomeData2 = incomeData %>%
  filter(Riding == "Groulx") %>%
  filter(Category == "Men" | Category == "Women")
totalWomen = incomeData2 %>%
  filter(Category == "Women") %>%
  filter(Constituency == "Women Aged 15+ By Level of Income in 2020") %>%
  pull(4)
totalMen = incomeData2 %>% 
  filter(Category == "Men") %>%
  filter(Constituency == "Men Aged 15+ By Level of Income in 2020") %>%
  pull(4)
incomeDataWomen = incomeData2 %>%
  filter(Category == "Women") %>%
  mutate(percentage = Population / totalWomen * 100) %>%
  slice(12:23)
incomeDataMen = incomeData2 %>%
  filter(Category == "Men") %>%
  mutate(percentage = Population / totalMen * 100) %>%
  slice(12:23)
combinedIncomes = full_join(incomeDataWomen, incomeDataMen)

ggplot(combinedIncomes, aes(x = 
      factor(Constituency, level = c("Less than $10,000", "$10,000 to $19,999", "$20,000 to $29,999", 
      "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999", 
      "$60,000 to $69,999", "$70,000 to $79,999", "$80,000 to $89,999",
       "$90,000 to $99,999", "$100,000 to $149,999", "$150,000+")), y = percentage, fill = Category)) + 
  geom_col(position = position_dodge()) +
  labs(x = "Income", y = "Percentage of Population", title = "Distribution of Wealth Between Men and Women in Groulx")

#Question 9 Now for questions that involve both politics and demographics! 
#Often times political parties intersect largely with class. Taking a look at 
#the income data supplied. Is there a party that seems to particularly appeal to 
#richer voters? Is there a party that seems to particularly appeal to poorer voters?

#Find which ridings have voters who belong to a lower class, find what party that 
#riding mostly votes for
meanIncome = incomeData %>%
  filter(Constituency == "Mean Total Income", Category == "People") %>%
  select(Riding, Constituency, Population)
ridingAndParty = electionData %>%
  select(PoliticalPartyAbbrev, PercentVotes, RidingName) %>%
  rename(Riding = RidingName)
incomeAndParty = full_join(meanIncome, ridingAndParty) %>%
  arrange(Population) %>%
  filter(PoliticalPartyAbbrev == "P.L.Q./Q.L.P." | PoliticalPartyAbbrev == "Q.S." | PoliticalPartyAbbrev == "P.Q." | PoliticalPartyAbbrev == "C.A.Q.-E.F.L.")
ggplot(incomeAndParty, aes(x = Population, y = PercentVotes, color = PoliticalPartyAbbrev)) + geom_point() +
  labs(x = "Mean Total Income")
incomeAndParty %>%
  filter(PoliticalPartyAbbrev == "P.L.Q./Q.L.P.") %>%
  ggplot(aes(x = Population, y = PercentVotes)) + geom_point() +
  labs(x = "Income", title = "Popularity of P.L.Q./Q.L.P. in Each Riding by Income")
incomeAndParty %>%
  filter(PoliticalPartyAbbrev == "Q.S.") %>%
  ggplot(aes(x = Population, y = PercentVotes)) + geom_point() +
  labs(x = "Income", title = "Popularity of Q.S. in Each Riding by Income")
incomeAndParty %>%
  filter(PoliticalPartyAbbrev == "P.Q.") %>%
  ggplot(aes(x = Population, y = PercentVotes)) + geom_point() +
  labs(x = "Income", title = "Popularity of P.Q. in Each Riding by Income")
incomeAndParty %>%
  filter(PoliticalPartyAbbrev == "C.A.Q.-E.F.L.") %>%
  ggplot(aes(x = Population, y = PercentVotes)) + geom_point() +
  labs(x = "Income", title = "Popularity of C.A.Q.-E.F.L. in Each Riding by Income")
#The P.L.Q./Q.L.P. party seems to appeal to both of the poorer and richer ridings

#Question 10 There is one party that is often said to mostly represent the 
#English-speaking minority in Quebec. Which party do you think this is? Why? 
#You can come up with any way to test this – visual, numerical, etc. But 
#make sure to explain and defend your idea!
englishData = languageData %>%
  filter(Category == "Primary Language", Constituency == "English")
totalPops = languageData %>%
  filter(Category == "Primary Language", Constituency == "Total Population by Primary Language Spoken")
englishAndPop = full_join(englishData, totalPops, by = "Riding") %>%
  select(Riding, Population.x, Population.y) %>%
  rename(EnglishSpeakingPop = Population.x) %>%
  rename(Population = Population.y) %>%
  filter(Riding != "Province") %>%
  mutate(Percentage = EnglishSpeakingPop / Population * 100)
englishAndParty = full_join(englishAndPop, ridingAndParty) %>%
  filter(PoliticalPartyAbbrev == "P.L.Q./Q.L.P." | PoliticalPartyAbbrev == "Q.S." | PoliticalPartyAbbrev == "P.Q." | PoliticalPartyAbbrev == "C.A.Q.-E.F.L.")
ggplot(englishAndParty, aes(x = Percentage, y = PercentVotes, color = PoliticalPartyAbbrev)) + geom_point() +
  labs(x = "Percentage of English Speakers")
#The P.L.Q./Q.L.P. party definitely appeals to the English speaking population

#Question 11 A man approaches you and gives you a challenge: His native 
#language is neither English nor French. He makes $100,000 a year. He lives 
#in an apartment building with more than 5 floors. He is 29 years old. He asks 
#you to guess what party he voted for based on these facts. What would be your 
#guess and why?
manLanguage = languageData %>%
  filter(Category == "Native Language") %>%
  filter(Constituency == "Unofficial Languages")
populationByRiding = ageData %>%
  filter(Constituency == "Total Population")
manLanguageAndPop = left_join(manLanguage, populationByRiding, by = "Riding") %>%
  mutate(percentage = Population.x / Population.y * 100)
manLanguageAndParty = full_join(manLanguageAndPop, ridingAndParty) %>%
  filter(Riding != "Province") %>%
  filter(PoliticalPartyAbbrev == "P.L.Q./Q.L.P." | PoliticalPartyAbbrev == "Q.S." | PoliticalPartyAbbrev == "P.Q." | PoliticalPartyAbbrev == "C.A.Q.-E.F.L.")
ggplot(manLanguageAndParty, aes(x = percentage, y =PercentVotes, color = PoliticalPartyAbbrev)) + geom_point() +
  labs(x = "Percentage of People whose Native Language is an Unofficial Language")
#P.L.Q./Q.L.P. for people who speak unofficial languages
manIncome = incomeData %>%
  filter(Constituency == "$100,000 to $149,999", Category == "Men", Riding != "Province")
manIncomeAndPop = left_join(manIncome, populationByRiding, by = "Riding") %>%
  mutate(percentage = Population.x / Population.y * 100)
manIncomeAndParty = full_join(manIncomeAndPop, ridingAndParty) %>%
  filter(Riding != "Province") %>%
  filter(PoliticalPartyAbbrev == "P.L.Q./Q.L.P." | PoliticalPartyAbbrev == "Q.S." | PoliticalPartyAbbrev == "P.Q." | PoliticalPartyAbbrev == "C.A.Q.-E.F.L.")
ggplot(manIncomeAndParty, aes(x = percentage, y = PercentVotes, color = PoliticalPartyAbbrev)) + geom_point() +
  labs(x = "Percentage of Men who make $100,000 to $149,999")
#C.A.Q.-E.F.L. but it's not clear for man who make 100,000
manHousing = householdData %>%
  filter(Constituency == "Apartment in a Building of 5 Floors or More", Riding != "Province")
manHousingAndPop = left_join(manHousing, populationByRiding, by = "Riding") %>%
  mutate(Population.x = as.numeric(Population.x)) %>%
  mutate(percentage = Population.x / Population.y * 100)
manHousingAndParty = full_join(manHousingAndPop, ridingAndParty) %>%
  filter(Riding != "Province") %>%
  filter(PoliticalPartyAbbrev == "P.L.Q./Q.L.P." | PoliticalPartyAbbrev == "Q.S." | PoliticalPartyAbbrev == "P.Q." | PoliticalPartyAbbrev == "C.A.Q.-E.F.L.")
ggplot(manHousingAndParty, aes(x = percentage, y = PercentVotes, color = PoliticalPartyAbbrev)) + geom_point() +
  labs(x = "Percentage of Population who Lives in an Apartment Building with 5 or More Floors")
#P.L.Q./Q.L.P.
manAge = ageData %>%
  filter(Constituency == "Population aged 15-29", Riding != "Province")
manAgeAndPop = left_join(manAge, populationByRiding, by = "Riding") %>%
  mutate(Population.x = as.numeric(Population.x)) %>%
  mutate(percentage = Population.x / Population.y * 100)
manAgeAndParty = full_join(manAgeAndPop, ridingAndParty) %>%
  filter(Riding != "Province") %>%
  filter(PoliticalPartyAbbrev == "P.L.Q./Q.L.P." | PoliticalPartyAbbrev == "Q.S." | PoliticalPartyAbbrev == "P.Q." | PoliticalPartyAbbrev == "C.A.Q.-E.F.L.")
ggplot(manAgeAndParty, aes(x = percentage, y = PercentVotes, color = PoliticalPartyAbbrev)) + geom_point() +
  labs(x = "Percentage of Population aged 15-29")
#P.L.Q./Q.L.P.