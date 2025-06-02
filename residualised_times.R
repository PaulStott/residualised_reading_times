rm(list=ls()) #clear workspace

#load packages
library(tidyverse)

#load data
WIDE_ANON <- read_csv("your_anon_data.csv") #load in some anonymised data in wide format

#convert to long format - assumes columns of interest are called CHUNK1, CHUNK2 etc.
#assumes there's a column with a reading time for each chunk e.g., C1.RT = RT for CHUNK1 etc.
#adapt as needed
LONG_ANON <- WIDE_ANON %>%
  pivot_longer(cols = starts_with("CHUNK"),
               names_to = "POS",
               values_to = "CHUNK",
               names_pattern = "CHUNK(\\d+)",
               names_transform = list(POS = as.integer)) %>%
  mutate(POS = as.character(POS)) %>%
  #group by Participant for individual RTs
  group_by(Participant) %>%
  #populate RT column with given chunk RT
  mutate(RT = case_when(POS == 1 ~ C1.RT,
                        POS == 2 ~ C2.RT,
                        POS == 3 ~ C3.RT,
                        POS == 4 ~ C4.RT)) %>%
  #add LEN(GTH) column for chunk length
  mutate(LEN = str_count(CHUNK, '\\w+'))

#make a copy of LONG_ANON with only filler trials
LONG_FILLERS <- LONG_ANON %>%
  filter(StimType == "fill")

#make a copy of LONG_ANON with only experimental trials
LONG_EXP <- LONG_ANON %>%
  filter(StimType == "exp") %>%
  mutate(PRED.RT = NA) #add new column PRED.RT to store predicted RT

#loop through each participant
for(participant in unique(LONG_FILLERS$Participant)) {
  #filter data for the current participant in the FILLERS dataset
  participant_data <- LONG_FILLERS %>% filter(Participant == participant)
  
  #fit an individual model for each participant: 
  #predicts chunk RT in the filler trial data as function of LEN + POS
  pred_mod <- lm(RT ~ LEN + POS, data = participant_data)

  #filter prediction data for current participant in experimental data
  prediction_data <- LONG_EXP %>% filter(Participant == participant)
  
  #predict chunk RT for participant in the experimental trial data,
  #given the model already fitted over the fillers data
  preds <- predict(pred_mod, newdata = prediction_data)

  #append predictions to dataframe
  LONG_EXP$PRED.RT[LONG_EXP$Participant == participant] <- preds
}

#convert back to wide format to export for analysis
WIDE_EXP <- LONG_EXP %>%
  dplyr::select(Participant, TrialN, POS, PRED.RT) %>%
  pivot_wider(names_from = POS,
              values_from = PRED.RT,
              names_glue = "C{POS}.PRED")

#join predicted RTs to experimental data
EXPERIMENTAL_DATA <- WIDE_ANON %>%
  filter(StimType == "exp") %>%
  left_join(WIDE_EXP, by = c("Participant", "TrialN"))

#calculate residualised reading times
EXPERIMENTAL_DATA <- EXPERIMENTAL_DATA %>%
  mutate(C1.RESID = C1.RT - C1.PRED,
         C2.RESID = C2.RT - C2.PRED,
         C3.RESID = C3.RT - C3.PRED,
         C4.RESID = C4.RT - C4.PRED)

#export dataset
EXPERIMENTAL_DATA %>%
  write_csv("your_experimental_data.csv")





