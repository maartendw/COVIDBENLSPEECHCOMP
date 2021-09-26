##################
#load the relevant data and packages
##################

#set work dir

setwd("C:/Users/maart/Documenten/HIRB/Fase 5/Methods of Corpus Linguistics/mcl/Corona_comparison")

# activate packages
library(tidyverse)
library(mclm)
library(ggplot2)

# retrieve list of function words
stop_list <- read_types("stop_listNL.txt") %>%
  print(n = 10)

# retrieve list of corpus files
corona_NL <- get_fnames("../Corona_NL") %>%
  print()
corona_BE <- get_fnames("../Corona_BE") %>%
  print()

# build frequency lists
corona_NL_flist <- freqlist(corona_NL) %>% 
  print(n = 100)
corona_BE_flist <- freqlist(corona_BE) %>%
  print(n = 100)

##################
#KEYWORD ANALYSIS#
##################

# keyword analysis Netherlands versus Belgium
scores_NL <- assoc_scores(corona_NL_flist, corona_BE_flist) %>%
  filter(!(type_names(.) %in% stop_list)) %>% 
  print(sort_order = "chi2_signed", n = 50)

myvars <- c("type", "PMI", "G_signed", "chi2_signed")

scores_NL_subset <- as.data.frame(scores_NL)[myvars] 

#write to csv for report
scores_NL_subset[with(scores_NL_subset, order(PMI, decreasing = TRUE)),][1:15,] %>% write.csv("C:/Users/maart/Documenten/HIRB/Fase 5/Methods of Corpus Linguistics/mcl/Corona_comparison/1.NL_PMI.csv", row.names = TRUE)
scores_NL_subset[with(scores_NL_subset, order(G_signed, decreasing = TRUE)),][1:15,]  %>% write.csv("C:/Users/maart/Documenten/HIRB/Fase 5/Methods of Corpus Linguistics/mcl/Corona_comparison/1.NL_GSIGN.csv", row.names = TRUE)
scores_NL_subset[with(scores_NL_subset, order(chi2_signed, decreasing = TRUE)),][1:15,] %>% write.csv("C:/Users/maart/Documenten/HIRB/Fase 5/Methods of Corpus Linguistics/mcl/Corona_comparison/1.NL_CHISQ.csv", row.names = TRUE)

# keyword analysis Belgium versus Netherlands
scores_BE <- assoc_scores(corona_BE_flist, corona_NL_flist) %>%
  filter(!(type_names(.) %in% stop_list)) %>%
  print(sort_order = "PMI", n = 50)

scores_BE_subset <- as.data.frame(scores_BE)[myvars]

#write to csv for report
scores_BE_subset[with(scores_BE_subset, order(PMI, decreasing = TRUE)),][1:15,] %>% write.csv("C:/Users/maart/Documenten/HIRB/Fase 5/Methods of Corpus Linguistics/mcl/Corona_comparison/1.BE_PMI.csv", row.names = TRUE)
scores_BE_subset[with(scores_BE_subset, order(G_signed, decreasing = TRUE)),][1:15,] %>% write.csv("C:/Users/maart/Documenten/HIRB/Fase 5/Methods of Corpus Linguistics/mcl/Corona_comparison/1.BE_GSIGN.csv", row.names = TRUE)
scores_BE_subset[with(scores_BE_subset, order(chi2_signed, decreasing = TRUE)),][1:15,] %>% write.csv("C:/Users/maart/Documenten/HIRB/Fase 5/Methods of Corpus Linguistics/mcl/Corona_comparison/1.BE_CHISQ.csv", row.names = TRUE)


#check for a keyword in context to see how we should classify it
conc(corona_NL, r"--[(?xi)  \b pakket \b  ]--") %>%
  print_kwic()
conc(corona_NL, r"--[(?xi)  \b ruim \b  ]--") %>%
  print_kwic()
conc(corona_NL, r"--[(?xi)  \b drukte \b  ]--") %>%
  print_kwic()
conc(corona_NL, r"--[(?xi)  \b klachten \b  ]--") %>%
  print_kwic()
conc(corona_NL, r"--[(?xi)  \b ggd \b  ]--") %>%
  print_kwic()
conc(corona_NL, r"--[(?xi)  \b advies \b  ]--") %>%
  print_kwic()
conc(corona_NL, r"--[(?xi)  \b thuis \b  ]--") %>%
  print_kwic()
conc(corona_NL, r"--[(?xi)  \b houden \b  ]--") %>%
  print_kwic()
conc(corona_BE, r"--[(?xi)  \b intensieve \b  ]--") %>%
  print_kwic()
conc(corona_BE, r"--[(?xi)  \b toepassing \b  ]--") %>%
  print_kwic()
conc(corona_BE, r"--[(?xi)  \b code \b  ]--") %>%
  print_kwic()

###############
#TIME ANALYSIS#
###############

#size = preferred fold  size
#total = total amount corpus files
#amt = amount of folds

#time analysis NL

size <- 7
total<- nrow(as.data.frame(corona_NL))
amt <- ceiling(total / size) %>% print()

for(i in 0:(amt-1)){
  print("This is timecluster")
  print(i+1)
  print("With the following target speeches")
  print(corona_NL[(1+size*i):(size+size*i)])
  coronaNL_window_flist <- freqlist(corona_NL[(1+size*i):(size+size*i)])
  coronaNL_reference_flist <- freqlist(corona_NL[-((1+size*i):(size+size*i))])
  assoc_scores(coronaNL_window_flist, coronaNL_reference_flist) %>%
    filter(!(type_names(.) %in% stop_list)) %>%
    print(sort_order = "G_signed", n = 50)}

#top 10 words for cluster 1 (9.3-25.3). MID= 17/03/2020: brabant, bruno, regeling, waardering, werk, bijzonder, rijn, problemen, maatregel, maatregelen
#top 10 words for cluster 2 (31.3-6.5). MID= 18/04/2020: kinderen, 28, april, mei, covid-19, 11, ankerpunten, paasweekend, stap, anderhalvemetersamenleving 
#top 10 words for cluster 3 (13.5-22.7). MID= 17/06/2020: juli, 1, juni, noodpakket, 15, vakantie, landen, drukte, koninkrijk, leerlingen
#top 10 words for cluster 4 (24.7-13.10). MID= 2/09/2020: regio, besmettingen, voorjaar, landelijke, virus, mensen, terug, amsterdam, uur, horeca
#top 10 words for cluster 5 (27.10-8-12). MID= 17/11/2020: kerst, december, januari, gedeeltelijke, dag, lockdown, extra, besmettingsgraad, vaccinatie, cadeau

# Read in the Dutch epidemiological data
IN_HOSP_NL <- read.csv2("IN_HOSP_NL.csv", 
                        header = TRUE, 
                        sep = ";", 
                        dec = ".",)

#omit missing values
IN_HOSP_NL<-na.omit(IN_HOSP_NL) 

#convert date to date object
IN_HOSP_NL$DATE <- as.Date(IN_HOSP_NL$DATE,format = "%d/%m/%y")

#plot the curve with the top key words per timefold
ggplot(data = IN_HOSP_NL, aes(x = DATE, y = IN_HOSPITAL)) +
  geom_line(color="grey", size=1)+
  geom_point(color="black", size=1)+
  annotate(geom="text", x=as.Date("17/03/2020",format = "%d/%m/%y"), y=725, label="brabant", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("17/03/2020",format = "%d/%m/%y"), y=675, label="bruno", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("17/03/2020",format = "%d/%m/%y"), y=625, label="regeling", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("17/03/2020",format = "%d/%m/%y"), y=575, label="waardering", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("17/03/2020",format = "%d/%m/%y"), y=525, label="werk", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("17/03/2020",format = "%d/%m/%y"), y=475, label="bijzonder", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("17/03/2020",format = "%d/%m/%y"), y=425, label="rijn", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("17/03/2020",format = "%d/%m/%y"), y=375, label="problemen", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("17/03/2020",format = "%d/%m/%y"), y=325, label="maatregel", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("17/03/2020",format = "%d/%m/%y"), y=275, label="maatregelen", color="dodgerblue4", size = 6) +
  
  annotate(geom="text", x=as.Date("18/04/2020",format = "%d/%m/%y"), y=675, label="kinderen", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("18/04/2020",format = "%d/%m/%y"), y=625, label="28", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("18/04/2020",format = "%d/%m/%y"), y=575, label="april", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("18/04/2020",format = "%d/%m/%y"), y=525, label="mei", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("18/04/2020",format = "%d/%m/%y"), y=475, label="covid-19", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("18/04/2020",format = "%d/%m/%y"), y=425, label="11", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("18/04/2020",format = "%d/%m/%y"), y=375, label="ankerpunten", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("18/04/2020",format = "%d/%m/%y"), y=325, label="paasweekend", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("18/04/2020",format = "%d/%m/%y"), y=275, label="stap", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("18/04/2020",format = "%d/%m/%y"), y=225, label="anderhalvemetersamenleving", color="turquoise3", size = 6) +
  
  annotate(geom="text", x=as.Date("17/06/2020",format = "%d/%m/%y"), y=725, label="juli", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("17/06/2020",format = "%d/%m/%y"), y=675, label="1", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("17/06/2020",format = "%d/%m/%y"), y=625, label="juni", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("17/06/20200",format = "%d/%m/%y"), y=575, label="noodpakket", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("17/06/2020",format = "%d/%m/%y"), y=525, label="15", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("17/06/2020",format = "%d/%m/%y"), y=475, label="vakantie", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("17/06/2020",format = "%d/%m/%y"), y=425, label="landen", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("17/06/2020",format = "%d/%m/%y"), y=375, label="drukte", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("17/06/2020",format = "%d/%m/%y"), y=325, label="koninkrijk", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("17/06/2020",format = "%d/%m/%y"), y=275, label="leerlingen", color="springgreen", size = 6) +
  
  annotate(geom="text", x=as.Date("2/09/2020",format = "%d/%m/%y"), y=675, label="regio", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("2/09/2020",format = "%d/%m/%y"), y=625, label="besmettingen", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("2/09/2020",format = "%d/%m/%y"), y=575, label="voorjaar", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("2/09/2020",format = "%d/%m/%y"), y=525, label="landelijke", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("2/09/2020",format = "%d/%m/%y"), y=475, label="virus", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("2/09/2020",format = "%d/%m/%y"), y=425, label="mensen", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("2/09/2020",format = "%d/%m/%y"), y=375, label="terug", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("2/09/2020",format = "%d/%m/%y"), y=325, label="amsterdam", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("2/09/2020",format = "%d/%m/%y"), y=275, label="uur", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("2/09/2020",format = "%d/%m/%y"), y=225, label="horeca", color="springgreen3", size = 6) +
  
  annotate(geom="text", x=as.Date("17/11/2020",format = "%d/%m/%y"), y=725, label="kerst", color="lawngreen", size = 6) +
  annotate(geom="text", x=as.Date("17/11/2020",format = "%d/%m/%y"), y=675, label="december", color="lawngreen", size = 6) +
  annotate(geom="text", x=as.Date("17/11/2020",format = "%d/%m/%y"), y=625, label="januari", color="lawngreen", size = 6) +
  annotate(geom="text", x=as.Date("17/11/2020",format = "%d/%m/%y"), y=575, label="gedeeltelijke", color="lawngreen", size = 6) +
  annotate(geom="text", x=as.Date("17/11/2020",format = "%d/%m/%y"), y=525, label="dag", color="lawngreen", size = 6) +
  annotate(geom="text", x=as.Date("17/11/2020",format = "%d/%m/%y"), y=475, label="lockdown", color="lawngreen", size = 6) +
  annotate(geom="text", x=as.Date("17/11/2020",format = "%d/%m/%y"), y=425, label="extra", color="lawngreen", size = 6) +
  annotate(geom="text", x=as.Date("17/11/2020",format = "%d/%m/%y"), y=375, label="besmettingsgraad", color="lawngreen", size = 6) +
  annotate(geom="text", x=as.Date("17/11/2020",format = "%d/%m/%y"), y=325, label="vaccinatie", color="lawngreen", size = 6) +
  annotate(geom="text", x=as.Date("17/11/2020",format = "%d/%m/%y"), y=275, label="cadeau", color="lawngreen", size = 6) +
  
  labs(x = "Date",
       y = "Hospital admissions",
       title = "Number of new hospitalizations per day since March in the Netherlands",
       subtitle = "Distinctive keywords per time interval.")+
  theme(plot.title = element_text(size = 20)) + 
  theme(axis.title.x = element_text(size = 20)) + 
  theme(axis.title.y = element_text(size = 20)) +
  theme_minimal(base_size = 22) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#time analysis BE
size <- 6
total<- nrow(as.data.frame(corona_BE))
amt <- ceiling(total / size) %>% print()

for(i in 0:(amt-1)){
  print("This is timecluster")
  print(i+1)
  print("With the following target speeches")
  print(corona_BE[(1+size*i):(size+size*i)])
  coronaBE_window_flist <- freqlist(corona_BE[(1+size*i):(size+size*i)])
  coronaBE_reference_flist <- freqlist(corona_BE[-((1+size*i):(size+size*i))])
  assoc_scores(coronaBE_window_flist, coronaBE_reference_flist) %>%
    filter(!(type_names(.) %in% stop_list)) %>%
    print(sort_order = "G_signed", n = 50)}

#top 10 words for cluster 1 (12.3-24.4). MID DATE = 2/4/20 : betalingsplan, openbaar, overheid, beproeving, vervoer, werkloosheid, federale, zelfstandigen, ondersteuning, België
#top 10 words for cluster 2 (6.5-24.6). MID DATE = 30/5/20 : leerlingen, juni, 8, juli, code, activiteiten, scenario, recht, aanwezig, onderwijs
#top 10 words for cluster 3 (15.7-16.10). MID DATE = 30/08/2020 : besmettingen, burgemeesters, maatregelen, plaatsen, barometer, grens, maatregel, onrechtvaardig, rekenen, treft
#top 10 words for cluster 4 (23.10-18.12). MID DATE = 19/11/2020 : één, ziekenhuizen, knuffelcontact, november, zorgverleners, weken, moment, zorg, reizen, december (dropped 'echt' sinds no real meaning)

# Read in the Belgian epidemiological data
IN_HOSP_BE <- read.csv2("IN_HOSP_BE.csv", 
                        header = TRUE, 
                        sep = ";", 
                        dec = ".",)

#omit missing values
IN_HOSP_BE<-na.omit(IN_HOSP_BE)
print(IN_HOSP_BE)

#convert date to date object
IN_HOSP_BE$DATE <- as.Date(IN_HOSP_BE$DATE,format = "%d/%m/%y")

#plot the curve with the top key words per timefold
ggplot(data = IN_HOSP_BE, aes(x = DATE, y = IN_HOSPITAL)) +
  geom_line(color="grey", size=1)+
  geom_point(color="black", size=1)+
  annotate(geom="text", x=as.Date("2/04/2020",format = "%d/%m/%y"), y=7250, label="betalingsplan", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("2/04/2020",format = "%d/%m/%y"), y=6750, label="openbaar", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("2/04/2020",format = "%d/%m/%y"), y=6250, label="overheid", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("2/04/2020",format = "%d/%m/%y"), y=5750, label="beproeving", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("2/04/2020",format = "%d/%m/%y"), y=5250, label="vervoer", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("2/04/2020",format = "%d/%m/%y"), y=4750, label="werkloosheid", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("2/04/2020",format = "%d/%m/%y"), y=4250, label="federale", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("2/04/2020",format = "%d/%m/%y"), y=3750, label="zelfstandigen", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("2/04/2020",format = "%d/%m/%y"), y=3250, label="ondersteuning", color="dodgerblue4", size = 6) +
  annotate(geom="text", x=as.Date("2/04/2020",format = "%d/%m/%y"), y=2750, label="België", color="dodgerblue4", size = 6) +
  
  annotate(geom="text", x=as.Date("30/05/2020",format = "%d/%m/%y"), y=6750, label="leerlingen", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("30/05/2020",format = "%d/%m/%y"), y=6250, label="juni", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("30/05/2020",format = "%d/%m/%y"), y=5750, label="8", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("30/05/2020",format = "%d/%m/%y"), y=5250, label="juli", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("30/05/2020",format = "%d/%m/%y"), y=4750, label="code", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("30/05/2020",format = "%d/%m/%y"), y=4250, label="activiteiten", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("30/05/2020",format = "%d/%m/%y"), y=3750, label="scenario", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("30/05/2020",format = "%d/%m/%y"), y=3250, label="recht", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("30/05/2020",format = "%d/%m/%y"), y=2750, label="aanwezig", color="turquoise3", size = 6) +
  annotate(geom="text", x=as.Date("30/05/2020",format = "%d/%m/%y"), y=2250, label="onderwijs", color="turquoise3", size = 6) +
  
  annotate(geom="text", x=as.Date("30/08/2020",format = "%d/%m/%y"), y=7250, label="besmettingen", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("30/08/2020",format = "%d/%m/%y"), y=6750, label="burgemeesters", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("30/08/2020",format = "%d/%m/%y"), y=6250, label="maatregelen", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("30/08/2020",format = "%d/%m/%y"), y=5750, label="plaatsen", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("30/08/2020",format = "%d/%m/%y"), y=5250, label="barometer", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("30/08/2020",format = "%d/%m/%y"), y=4750, label="grens", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("30/08/2020",format = "%d/%m/%y"), y=4250, label="maatregel", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("30/08/2020",format = "%d/%m/%y"), y=3750, label="onrechtvaardig", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("30/08/2020",format = "%d/%m/%y"), y=3250, label="rekenen", color="springgreen", size = 6) +
  annotate(geom="text", x=as.Date("30/08/2020",format = "%d/%m/%y"), y=2750, label="treft", color="springgreen", size = 6) +
  
  annotate(geom="text", x=as.Date("19/11/2020",format = "%d/%m/%y"), y=7000, label="één", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("19/11/2020",format = "%d/%m/%y"), y=6500, label="ziekenhuizen", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("19/11/2020",format = "%d/%m/%y"), y=6000, label="knuffelcontact", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("19/11/2020",format = "%d/%m/%y"), y=5500, label="november", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("19/11/2020",format = "%d/%m/%y"), y=5000, label="zorgverleners", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("19/11/2020",format = "%d/%m/%y"), y=4500, label="weken", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("19/11/2020",format = "%d/%m/%y"), y=4000, label="moment", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("19/11/2020",format = "%d/%m/%y"), y=3500, label="zorg", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("19/11/2020",format = "%d/%m/%y"), y=3000, label="reizen", color="springgreen3", size = 6) +
  annotate(geom="text", x=as.Date("19/11/2020",format = "%d/%m/%y"), y=2500, label="december", color="springgreen3", size = 6) +
  labs(x = "Date",
       y = "People in hospital",
       title = "Number of hospital beds occupied since March in Belgium",
       subtitle = "Distinctive keywords per time interval.")+
  theme(plot.title = element_text(size = 20)) + 
  theme(axis.title.x = element_text(size = 20)) + 
  theme(axis.title.y = element_text(size = 20)) +
  theme_minimal(base_size = 22, ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


###########
#SCRAP - INSPIRATION FOR FUTURE RESEARCH
###########

# details inspection of the keyword analysis
scores_NL <- assoc_scores(corona_NL_flist, corona_BE_flist) 
scores_BE <- assoc_scores(corona_BE_flist, corona_NL_flist)

scores_NL %>% 
  filter(type_names(.) == "onderwijs")
scores_BE %>% 
  filter(type_names(.) == "europese")


scores_NL %>% filter(type_names(.) %in% 
                          c("mondkapje", "mondmasker", "lockdown", "gevaar", "piek", "symptomen"))

scores_BE %>% filter(type_names(.) %in% 
                          c("mondmasker", "lockdown", "gevaar", "piek", "symptomen"))

######################
#COLLOCATION ANALYSIS#
######################

# collocation analysis  
NL_gevaar <- surf_cooc(corona_NL, r"--[(?xi) .* piek .* ]--") %>% 
  assoc_scores() %>%
  print(sort_order = "G_signed")

BE_gevaar <- surf_cooc(corona_BE, r"--[(?xi) .* piek .* ]--") %>% 
  assoc_scores() %>%
  print(sort_order = "G_signed")

# concordances for "gevaar" 
NL_gevaar_conc <- conc(corona_NL, r"--[(?xi) \b gevaar \b ]--") %>% 
  print_kwic()
BE_gevaar_conc <- conc(corona_BE, r"--[(?xi) \b gevaar \b ]--") %>% 
  print_kwic()