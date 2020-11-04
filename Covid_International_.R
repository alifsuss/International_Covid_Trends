#TOOLS AND STUFF
if (!require("pacman")) install.packages("pacman")
setwd("~/R/Covid-data")
pacman::p_load(datasets, pacman, tidyverse, rio, dplyr, ggplot2, magrittr, hrbrthemes, extrafont)

#DECLARE COUNTRY NAMES ACCORDING TO THE 3-LETTERS COUNTRY CODE, NEGARA = COUNTRY in INDONESIAN, SO
negara = c("IDN","ITA","FRA","GBR","USA","MYS","KOR")    #ADD COUNTRY THAT YOU'RE INTERESTED IN
today.date <- Sys.Date() 

#IMPORTING ONLINE DATA FROM EUROPEAN OPEN DATA
dfraw <-import("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/data.csv")

#MAKE NEW DATAFRAME ACCORDING TO df_raw SO IF WE CAN ALWAYS HAVE THE RAW DATA, AND SAVE IT AS TIBBLE
df <- dfraw
as_tibble(df)

#FORMAT THE dateRep column TO Date Type <- IT WAS CHARACTER BEFORE, SO WE CAN PLOT IT LATER
df$dateRep <- as.Date(df$dateRep, format = "%d/%m/%y") #SETTING UP THE dateRep AS DATE

#ONLY SELECT dateRep as DATE, countryterritoryCode as CODE, and cases, DELETE FUTURE DATE (THIS DATE IS DUE TO ERROR FROM THE SOURCE)
df %<>% 
  select(date = dateRep, code = countryterritoryCode, cases) %>%
  filter(date <= today.date) %>% #this to delete every date after today
  print()
#ARRANGE CODE SO IT WILL BE ARRANGED AS CODE FIRST, AND WITHIN THE CODE THE DATE WILL BE ARRANGED, this in ASCENDING
df %<>%
  arrange(code, date)

#CONVERT THE df$cases AS NUMERIC
df$cases <- as.numeric(as.character(df$cases))

#MAKE DATABASES with format df_* FOLLOWED BY COUNTRY 3-LETTERS CODE, FILTERED ACCORDING TO THE 3-LETTER CODE
for (i in 1:length(negara)) {
  assign(paste0("df_",negara[i]), filter(df, str_detect(code, negara[i])))
}

#THIS MAY BE TOO MUCH CODE, SINCE I DID IT ONE BY ONE, BUT ADDING A COUNTRY WONT BE DIFFICULT
#JUST COPY THE FOLLOWING:
#---------------------------
#df_XXX %<>%   
#  mutate(intotal = cumsum(cases)) %>%    #--> THIS WILL CALCULATE THE CUMULATIVE CASES AND STORE IT IN intotal
#  select(date, daily = cases, intotal, code) %>%  #--->THIS WILL SELECT date, cases AS daily, intotal and code
#  print()    # --> JUST TO SEE IF YOUR OPERATIONS ARE WORKING
#---------------------------
#AND CHANGE THE XXX WITH THE 3- LETTER COUNTRY CODE THAT YOU WANT TO LOOK, BUT REMEMBER TO ADD THE 3-LETTER in negera first

df_FRA %<>%   
  mutate(intotal = cumsum(cases)) %>%
  select(date, daily = cases, intotal, code) %>%
  print()

df_GBR %<>%   
  mutate(intotal = cumsum(cases)) %>%
  select(date, daily = cases, intotal, code) %>%
  print()

df_IDN %<>%   
  mutate(intotal = cumsum(cases)) %>%
  select(date, daily = cases, intotal, code) %>%
  print()

df_ITA %<>%   
  mutate(intotal = cumsum(cases)) %>%
  select(date, daily = cases, intotal, code) %>%
  print()

df_KOR %<>%   
  mutate(intotal = cumsum(cases)) %>%
  select(date, daily = cases, intotal, code) %>%
  print()

df_MYS %<>%   
  mutate(intotal = cumsum(cases)) %>%
  select(date, daily = cases, intotal, code) %>%
  print()

df_USA %<>%   
  mutate(intotal = cumsum(cases)) %>%
  select(date, daily = cases, intotal, code) %>%
  print()

#MAKE NEW DATAFRAME FROM ALL THE DATAFRAMES AVAILABLE, IF ANY ADD the df_XXX FROM PREVIOUS OPERATION
df_comb <- rbind(df_FRA, df_GBR, df_IDN, df_ITA, df_KOR, df_MYS, df_USA)  #column names should be the same
df_comb$code <- as.factor(df_comb$code)             #change the code as factor FOR PLOTTING LATER

#THIS TO PLOT THE GRAPH
ggplot(df_comb, aes(x = intotal, y = daily , color = factor(code))) +
  geom_line(size = .5, alpha = 0.6) + 
  geom_point(size = 3, alpha = 0.6)  + 
  ggtitle(paste('COVID-19 trends of', today.date, sep = " ")) +
  theme(
    axis.line = element_line(size = 1),
    axis.text=element_text(size=15),
    axis.title=element_text(size=15,face="bold"),
    legend.text=element_text(size=17),
    legend.justification=c(1,1),
    legend.position=c(0.4,0.95),
    legend.title= element_text("COUNTRY", size = 17, face = "bold"),
    panel.background = element_rect(fill = "white", colour = "grey50"),
  ) + 
  scale_color_manual( name = "COUNTRY",
    values = c("black", "red", "chartreuse3", "blue", "cyan2", "purple", "darkorange"),
    labels = c("FRANCE","UK", "INDONESIA", "ITALY", "KOREA", "MALAYSIA", "USA")
  ) +
  xlab("TOTAL INFECTED") +
  ylab("DAILY CASES") +
  scale_y_continuous(trans = "log10") + 
  scale_x_continuous(limits = c(100, NA), trans = "log10") +
  ggsave(filename = paste0("Countries_", format(Sys.time(), "%Y-%m-%d_%H-%M", tz = "Europe/London"),".jpg"), width = 5.3 , height = 8.49, dpi = 300, units = "in")

#rm(list = ls())
#ENJOY!