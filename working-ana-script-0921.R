# Load libraries ----
library(tidyverse)
library (afex)
library(emmeans)

# Read in ----
#setwd("C:/Users/kkompatsiari/Dropbox/IIT/phd/Experiments/Hyperscanning/HyperscanningRproject")
pilot_path = paste(getwd(), "/raw data/", sep = "")
list_of_datanames = list.files(pilot_path)
dat_exp = lapply(paste(pilot_path, list_of_datanames, sep = ""), read.csv)
dat_exp = do.call(plyr::rbind.fill, dat_exp)

# Manual entry of subjective ratings of participants 
dat_exp$RespInclusionRight[which(dat_exp$participant=="41_42Exp")] = 3
dat_exp$RespInclusionRight[which(dat_exp$participant=="43_44Exp")] = 2
dat_exp$RespInclusionRight[which(dat_exp$participant=="69_70Exp")] = 2



# preprocessing ----

dat_preprocessed = dat_exp %>%
  fill(RespInclusionLeft, .direction = "up") %>%
  fill(RespInclusionRight, .direction = "up") %>%
  group_by(
    participant) %>% 
  # get rid of practice trials
  slice(
    -c(1:2)) %>% 
  # we need to recode to 1 and 2 because the fill function won't work for characters. 
  # It only works for numeric variables
  mutate(
    RespTuringLeft = 
      case_when(RespTuringLeft == "c" ~ 1,
                RespTuringLeft == "h" ~ 2),
    RespTuringRight = 
      case_when(RespTuringRight == "c" ~ 1,
                RespTuringRight == "h" ~ 2),
    ) %>%
  fill(
    RespTuringLeft, .direction = "up") %>%
  fill(
    RespTuringRight, .direction = "up") %>%
  fill(
    keyTuringLeft.corr, .direction = "up") %>%
  fill(
    keyTuringRight.corr, .direction = "up") %>%
  fill(
    RespInclusionLeft, .direction = "up") %>%
  fill(
    RespInclusionRight, .direction = "up") %>%
  mutate(
    RespTuringLeft = recode(
      RespTuringLeft, `1` = "comp", `2` = "human"),
    RespTuringRight = recode(
      RespTuringRight, `1` = "comp", `2` = "human")) %>%
  mutate(
    CorrTuringLeft = case_when(RespTuringLeft == "comp" & 
                                 CorrectTuringReply == "c" ~ 1,
                               RespTuringLeft == "comp" &
                                 CorrectTuringReply == "h" ~ 0,
                               RespTuringLeft == "human" &
                                 CorrectTuringReply == "h" ~ 1,
                               RespTuringLeft == "human" & 
                                 CorrectTuringReply == "c" ~ 0),
    CorrTuringRight = case_when(RespTuringRight == "comp" & 
                                  CorrectTuringReply == "c" ~ 1,
                                RespTuringRight == "comp" &
                                  CorrectTuringReply == "h" ~ 0,
                                RespTuringRight == "human" &
                                  CorrectTuringReply == "h" ~ 1,
                                RespTuringRight == "human" & 
                                  CorrectTuringReply == "c" ~ 0)) %>%
  mutate(
    CorrTuringDyad = case_when(CorrTuringLeft == 1 & 
                                 CorrTuringRight == 1 ~ 1,
                               CorrTuringLeft == 0 & 
                                 CorrTuringRight == 1 ~ 0,
                               CorrTuringLeft == 1 & 
                                 CorrTuringRight == 0 ~ 0,
                               CorrTuringLeft == 0 & 
                                 CorrTuringRight == 0 ~ 0),
    #create a full trial number
    trial_num = row_number()
  ) %>%
  ## Delete every 11th row
  filter(
    is.na(Number) == F,
    is.na(key_respLeft.rt) == F,
    is.na(key_respRight.rt) == F
  ) %>%
  mutate(
    # create lead/lags for each participant
    choice = case_when(RespTuringRight == "comp" &
                         RespTuringLeft == "comp" ~ 
                         "comp_congruent",
                       RespTuringRight == "comp" & 
                         RespTuringLeft == "human" ~ 
                         "incongruent",
                       RespTuringRight == "human" & 
                         RespTuringLeft == "comp" ~ 
                         "incongruent",
                       RespTuringRight == "human" & 
                         RespTuringLeft == "human" ~
                         "human_congruent"),
    CongruencyChoice = case_when(RespTuringRight == "comp" &
                                   RespTuringLeft == "comp" ~ 
                                   "congruent",
                                 RespTuringRight == "comp" & 
                                   RespTuringLeft == "human" ~ 
                                   "incongruent",
                                 RespTuringRight == "human" & 
                                   RespTuringLeft == "comp" ~ 
                                   "incongruent",
                                 RespTuringRight == "human" & 
                                   RespTuringLeft == "human" ~
                                   "congruent"),
    closeness_diff = abs(RespInclusionLeft - RespInclusionRight),
    closeness_avg = mean(c(RespInclusionLeft, RespInclusionRight)
    )) %>% 
  group_by(
    participant, Block.thisN) %>%
  mutate(good_trials_n = n()) %>%
  filter(good_trials_n > 8) %>%
  ungroup(participant, Block.thisN) %>%
  filter(key_respLeft.rt > 4,
         key_respRight.rt > 4) %>%
  mutate(
    # measure synchrony
    sync_value = abs((key_respLeft.rt - key_respRight.rt))/
      (key_respLeft.rt+key_respRight.rt)*100
    ) %>% 
  mutate(
    # calculate the difference between Rtleft and Computer
    sync_value_Comp_Left = abs((key_respLeft.rt-Comp))/
      (key_respLeft.rt+Comp)*100,
    # calculate the difference between Rtright and Computer
    sync_value_Comp_Right = abs((Comp-key_respRight.rt))/
      (Comp+key_respRight.rt)*100) %>% 
  mutate(
    # calculate an average value between this difference
    sync_value_Comp = (sync_value_Comp_Left+sync_value_Comp_Right)/2) %>% 
  mutate(
    # create a variable that has the value of the diff with computer when the partner is computer and the diff with the other human when the partner is human
    sync_mixed_Comp_Human = case_when(Partner == "Computer"  ~ 
                                        sync_value_Comp,
                                      Partner == "Human" ~ 
                                        sync_value)) %>%
  mutate(which_block = ifelse(Block.thisN < 10, "First_half", "Second_half")) %>%
  mutate(bad_participants = ifelse(participant %in% c(
    #left handed participants
    #"51_52Exp", "55_56Exp", "63_64Exp",
    # low trial counts
    "39_40Exp", "41_42Exp", "73_74Exp",
    # RT above 4 seconds
    "33_34ExpCorr", "21_22Exp"), "bad", "good")) %>%
  filter(
    !participant %in% c("41_42Exp", "33_34ExpCorr", "73_74Exp"
  ))
  




# get rid of bad pees ----
# dat_clean = dat_preprocessed %>%
#   #left handed participants
#   filter(
#     !participant %in% c("51_52Exp", "55_56Exp", "63_64Exp",
#   # low trial counts
#     "39_40Exp", "41_42Exp", "73_74Exp",
#   # RT above 4 seconds
#   "33_34ExpCorr", "21_22Exp"))


# check number of blocks per choice
data.frame(dat_preprocessed %>% group_by(participant, Block.thisN, choice) %>%
                         summarize(tt = n()))
  
  
# plots ----
dat_correl = dat_clean %>%
  group_by(participant,Partner, choice) %>%
  summarise(ccf_values = ccf(as.numeric(ts(key_respLeft.rt)),as.numeric(ts(key_respRight.rt)), lag.max=00)$acf) #in acf object the correlation values are saved

# for correlation between InclusionDiff and Partner
dat_correl_HumanDiffQuest = dat_clean %>%
  filter(Partner=="Human") %>%
  group_by(participant) %>%
  summarise(avg_sync = mean(sync_value),
            InclusionQuest = mean(closeness_diff))


dat_correl_ComputerDiffQuest = dat_clean %>%
  filter(Partner=="Computer") %>%
  group_by(participant) %>%
  summarise(avg_sync = mean(sync_value),
            InclusionQuest = mean(closeness_diff))
    
resHumanDiff <- cor.test(dat_correl_HumanDiffQuest$avg_sync, dat_correl_HumanDiffQuest$InclusionQuest, 
                method = "pearson")

resHumanComputer <- cor.test(dat_correl_ComputerDiffQuest$avg_sync, dat_correl_ComputerDiffQuest$InclusionQuest, 
                         method = "pearson")

# for correlation between Inclusion Average and Partner
dat_correl_HumanAvgQuest = dat_clean %>%
  filter(Partner=="Human") %>%
  group_by(participant) %>%
  summarise(avg_sync = mean(sync_value),
            InclusionQuest = mean(closeness_avg))


dat_correl_ComputerAvgQuest = dat_clean %>%
  filter(Partner=="Computer") %>%
  group_by(participant) %>%
  summarise(avg_sync = mean(sync_value),
            InclusionQuest = mean(closeness_avg))

resHumanAvg <- cor.test(dat_correl_HumanAvgQuest$avg_sync, dat_correl_HumanAvgQuest$InclusionQuest, 
                         method = "pearson")

resHumanComputerAvg <- cor.test(dat_correl_ComputerAvgQuest$avg_sync, dat_correl_ComputerAvgQuest$InclusionQuest, 
                             method = "pearson")

    

#  correlation for choice
dat_correl_HumanChoiceQuest = dat_clean %>%
  filter(choice=="human_congruent") %>%
  group_by(participant) %>%
  summarise(avg_sync = mean(sync_value),
            InclusionQuest = mean(closeness_avg))


dat_correl_ComputerChoiceQuest = dat_clean %>%
  filter(choice=="comp_congruent") %>%
  group_by(participant) %>%
  summarise(avg_sync = mean(sync_value),
            InclusionQuest = mean(closeness_avg))

# for correlation between 
dat_correl_IncognruentChoiceQuest = dat_clean %>%
  filter(choice=="incongruent") %>%
  group_by(participant) %>%
  summarise(avg_sync = mean(sync_value),
            InclusionQuest = mean(closeness_avg))

resHumanChoiceAvg <- cor.test(dat_correl_HumanChoiceQuest$avg_sync, dat_correl_HumanChoiceQuest$InclusionQuest, 
                        method = "pearson")

resCompChoiceAvg <- cor.test(dat_correl_ComputerChoiceQuest$avg_sync, dat_correl_ComputerChoiceQuest$InclusionQuest, 
                                method = "pearson")


resCompIncongruentAvg <- cor.test(dat_correl_IncognruentChoiceQuest$avg_sync, dat_correl_IncognruentChoiceQuest$InclusionQuest, 
                             method = "pearson")

dat_correl = dat_preprocessed %>%
  group_by(participant,Partner) %>%
  summarise(ccf_values = ccf(as.numeric(ts(key_respLeft.rt)),as.numeric(ts(key_respRight.rt)), lag.max=10)$acf) #in acf object the correlation values are saved

dat_sum_correl = dat_correl %>%
  group_by(Partner, choice) %>%
  summarise(mean_ccf_values = mean(ccf_values))

dat_std = dat_preprocessed %>%
  group_by(participant,Partner, pchoice) %>%
  summarise(data_std_values_left = var(key_respLeft.rt), 
            data_std_values_right = var(key_respRight.rt))%>%
  pivot_longer(cols = c(data_std_values_left, data_std_values_right), names_to = "cazzo")

dat_preprocessed %>%
  ggplot(aes(Partner, sync_value, color = choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  facet_wrap(~bad_participants, scales = "free_y")


dat_std %>%
  ggplot(aes(y = data_std_values_right, x = Partner, color = choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("STD value by partner")


dat_correl %>%
  ggplot(aes(y = ccf_values, x = Partner, color = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Corss correlation valye by partner")


dat_correl %>%
  ggplot(aes(y = ccf_values, x = Partner, color = choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  scale_y_continuous(breaks = seq(.4,1.0,.05))+
  labs("Corss correlation valye by partner and particpnt's choice")


dat_std %>%
  ggplot(aes(y = value, x = Partner, color = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Corss correlation valye by partner")




dat_clean %>%
  group_by(participant, Partner, Block.thisN) %>% 
  filter(participant %in% c("1_2Exp","3_4", "5_6Exp",
                            "7_8Exp", "9_10Exp", "11_12Exp",
                            "13_14Exp", "15_16Exp", "17_18Exp",
                            "19_20Exp","21_22Exp","23_24Exp",
                            "25_26Exp","27_28Exp","29_30Exp", 
                            "31_32Exp","33_34ExpCorr","35_36Exp", 
                            "37_38Exp","39_40Exp" )) %>% 
  summarize(choice = first(choice)) %>%
  ggplot(aes(x = Partner,fill=choice))+
  geom_histogram(stat="count", position = position_dodge(0.9), 
                 color = "black",
                 lwd = 1, alpha = .8)+theme_bw()+
  geom_text(stat='count', aes(label=round(..count..)), 
            position = position_dodge(0.9), vjust=1) +
  theme(text = element_text(size = 10))+
  labs(title = "number of Blocks per partner per first half of couples") +
  facet_wrap(~participant, ncol=4)


dat_preprocessed %>%
  group_by(participant, Partner) %>% 
  ggplot(aes(x = Partner,fill=Partner))+
  geom_histogram(stat="count", position = position_dodge(0.9), 
                 color = "black",
                 lwd = 1, alpha = .8)+theme_bw()+
  geom_text(stat='count', aes(label=round(..count..)), 
            position = position_dodge(0.9), vjust=1) +
  theme(text = element_text(size = 10))+
  labs(title = "number of Trials  per partner per Participant") +
  facet_wrap(~participant)


dat_clean %>%
  group_by(participant, Partner, Block.thisN) %>% 
  filter(participant %in% c("41_42Exp","43_44Exp", "45_46Exp",
                            "47_48Exp", "49_50Exp", "51_52Exp",
                            "53_54Exp", "55_56Exp", "57_58Exp",
                            "59_60Exp","61_62Exp","63_64Exp",
                            "65_66Exp","67_68Exp","69_70Exp", 
                            "71_72Exp","73_74Exp","75_76Exp", 
                            "77_78Exp","79_80Exp","81_82Exp",
                            "83_84Exp" )) %>% 
  summarize(choice = first(choice)) %>%
  ggplot(aes(x = Partner, fill=choice))+
  geom_histogram(stat="count", position = position_dodge(0.9), 
                 color = "black",
                 lwd = 1, alpha = .8)+theme_bw()+
  geom_text(stat='count', aes(label=round(..count..)), 
            position = position_dodge(0.9), vjust=1) +
  theme(text = element_text(size = 10))+
  labs(title = "number of Blocks per partner per second half of couples") +
  facet_wrap(~participant, ncol=4)


dat_preprocessed %>%
  group_by(participant, Partner, Block.thisN) %>% 
  summarize(choice = first(choice)) %>%
  ggplot(aes(x = Partner,  fill= choice))+
  geom_histogram(stat='count', position = position_dodge(.9), 
                 color = "black",
                 lwd = 1, alpha = .8)+theme_bw()+
  geom_text(stat='count', aes(label=round(..count..)), 
            position = position_dodge(.9), vjust=2) +
  labs(title = "number of choices per Partner for all participants")

dat_preprocessed %>%
  group_by(participant, Partner, Block.thisN) %>% 
  summarize(choice = first(choice)) %>%
  ggplot(aes(x = choice,  fill= choice))+
  geom_histogram(stat='count', position = position_dodge(.9), 
                 color = "black",
                 lwd = 1, alpha = .8)+theme_bw()+
  geom_text(stat='count', aes(label=round(..count..)), 
            position = position_dodge(.9), vjust=2) +
  labs(title = "number of choices for all participants")


dat_preprocessed %>%
  mutate(new_block = ifelse(Block.thisN<10, "first", "second")) %>%
  ggplot(aes(y = sync_value, x = Partner, color = choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync valye for each agent by participant choice")+
  facet_wrap(~new_block)

dat_clean %>%
  mutate(new_block = ifelse(Block.thisN<10, "first", "second")) %>%
  ggplot(aes(y = sync_value, x = Partner, color = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync valye for each agent by participant choice")+
  facet_wrap(~new_block)

dat_clean %>%
  ggplot(aes(y = sync_value, x = Block.thisN, color = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  geom_smooth()+
  theme_bw()+
  labs("Sync valye for each agent by participant choice")

dat_clean %>%
  ggplot(aes(key_respLeft.rt))+
  geom_histogram()

dat_clean %>%
  ggplot(aes(key_respRight.rt))+
  geom_histogram()


dat_preprocessed %>%
  ggplot(aes(y = sync_value, x = Partner, color = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync valye for each agent by participant choice")
  #facet_wrap(~participant)


dat_clean %>%
  ggplot(aes(y = sync_value, x = Partner, color = CongruencyChoice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()
#+facet_wrap(~participant)+
  labs("Sync valye for each agent by Congruency and partner")

for(i in dat_clean$participant){
  my_plots[[i]] = dat_clean %>%
    filter(participant == i) %>%
    ggplot(aes(y = sync_value, x = Partner, color = choice))+
    stat_summary(fun.data = mean_se, geom = "errorbar", 
                 width = .1, position = position_dodge(.3),
                 fun.args = list(mult = 1.96))+
    stat_summary(fun.data = mean_se, geom = "point", size = 5, 
                 position = position_dodge(.3))+
    theme_bw()+
    labs("Sync valye for each agent by Congruency and partner")}

dat_clean %>%
  ggplot(aes(y = sync_z_value, fill = CongruencyChoice))+
  geom_histogram()+
  theme_bw()+facet_wrap(~Partner)+
  labs("z Sync valye for each agent by Congruency and partner")

dat_preprocessed %>%
  ggplot(aes(y = sync_value, x = CongruencyChoice, color = CongruencyChoice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync valye for each agent by Congruency")

dat_preprocessed %>%
  ggplot(aes(y = sync_value, x = Partner, color = choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync valye by participant choice")

dat_clean %>%
  ggplot(aes(y = CorrTuringDyad, x = Partner, color = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Average Correctness in Turing Test by Partner")

# try a different way for correctness based on the block (same results)
dat_preprocessed %>%
  group_by(participant, Partner, Block.thisN) %>%
  summarize(CorrTuringDyad=first(CorrTuringDyad)) %>%
  ggplot(aes(y = CorrTuringDyad, x = Partner, color = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Average Correctness in Turing Test by Partner")

# trying to plot instances of
dat_preprocessed %>%
  group_by(participant, Partner, Block.thisN, choice) %>%
  summarize(choice=first(choice)) %>%
  ggplot(aes(y = choice, x = Partner, color = Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Average Correctness in Turing Test by Partner")



dat_preprocessed %>%
  ggplot(aes(y = sync_value, x = Partner, color=Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync valye for each actual partner")

dat_preprocessed %>%
  ggplot(aes(y = sync_value, x = Partner, color=choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync valye per choice")+
  facet_wrap(~participant)

dat_clean %>%
  ggplot(aes(y = sync_value, x = CorrTuringDyad))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync valye for predicting correct the partner")

dat_clean %>%
  ggplot(aes(y = sync_value, x = Partner, color=as.factor(CorrTuringDyad )))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync valye for predicting correct the partner")



dat_clean %>%
  ggplot(aes(y = sync_mixed_Comp_Human, x = Partner, color=Partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync mixed comp human value for each actual partner")

dat_clean %>%
  ggplot(aes(y = sync_mixed_Comp_Human, x = Partner, color=choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync mixed comp human value for each actual partner and Choice")


dat_clean %>%
  #filter(choice %in% c("comp_congruent", "human_congruent")) %>%
  # group_by(participant, Partner, choice) %>%
  # mutate(correct_split = ifelse(CorrTuringDyad > .35, "Hi", "Lo")) %>%
  ggplot(aes(y = sync_value, x = as.factor(CorrTuringDyad) , color = Partner ))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  labs("Sync valye for predicting correct the partner")



dat_clean %>%
  #filter(choice %in% c("comp_congruent", "human_congruent")) %>%
  # group_by(participant, Partner, choice) %>%
  # mutate(correct_split = ifelse(CorrTuringDyad > .35, "Hi", "Lo")) %>%
  ggplot(aes(y = sync_value, x = Partner, color=Partner ))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()+
  facet_wrap(~CorrTuringDyad)+
  labs("Sync valye for correctness of dyad and the partner")
  
  
  
  dat_clean %>%
    filter(choice %in% c("comp_congruent", "human_congruent")) %>%
    # group_by(participant, Partner, choice) %>%
    # mutate(correct_split = ifelse(CorrTuringDyad > .35, "Hi", "Lo")) %>%
    ggplot(aes(y = sync_value, x = as.factor(CorrTuringDyad), color=as.factor(CorrTuringDyad) ))+
    stat_summary(fun.data = mean_se, geom = "errorbar", 
                 width = .1, position = position_dodge(.3),
                 fun.args = list(mult = 1.96))+
    stat_summary(fun.data = mean_se, geom = "point", size = 5, 
                 position = position_dodge(.3))+
    theme_bw()+
  labs("Sync valye for correctness of dyad and the partner")


  
  
  dat_clean %>%
    filter(dat_clean$choice %in% c("comp_congruent", "human_congruent")) %>%
    #(participant, Partner, choice) %>%
    # mutate(correct_split = ifelse(CorrTuringDyad > .35, "Hi", "Lo")) %>%
    ggplot(aes(y = sync_z_value, x = Partner, color=choice))+
    stat_summary(fun.data = mean_se, geom = "errorbar", 
                 width = .1, position = position_dodge(.3),
                 fun.args = list(mult = 1))+
    stat_summary(fun.data = mean_se, geom = "point", size = 5, 
                 position = position_dodge(.3))+
    theme_bw()+
    labs("Sync valye for correctness of dyad and the partner")
  
dat_clean %>%
  ggplot(aes(y = sync_value, x = Partner, color = choice))+
  geom_point(shape = 1, alpha = .8, position = position_jitterdodge(.1))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.8),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.8))+
  theme_bw()+
  labs("Sync valye for each agent by participant choice with individual data points")

dat_clean %>%
  ggplot(aes(sync_value, closeness_diff, color = Partner))+
  geom_point()+
  geom_smooth(method = "lm", se =F)+theme_bw()


dat_clean %>%
  ggplot(aes(sync_value, closeness_diff, color = choice))+
  geom_point()+
  geom_smooth(method = "lm", se =F)+theme_bw()

dat_clean %>%
  ggplot(aes(sync_value, closeness_diff, color = CongruencyChoice))+
  geom_point()+
  geom_smooth(method = "lm", se =F)+theme_bw()

dat_clean %>%
  ggplot(aes(sync_value, closeness_avg, color = Partner))+
  geom_point()+
  geom_smooth(method = "lm", se =F)+theme_bw()

dat_clean %>%
  ggplot(aes(sync_value, closeness_avg, color = choice))+
  geom_point()+
  geom_smooth(method = "lm", se =F)+theme_bw()


# stats ----

dat_clean$choice=as.factor(dat_clean$choice)
dat_clean$Partner=as.factor(dat_clean$Partner)


res_aov1 <- aov(sync_value ~ Partner,
                data = dat_clean
)

res_aov2 <- aov(sync_value ~ Partner+choice,
                data = dat_clean
)
summary(res_aov2)


library(multcomp)
# Tukey HSD test:
post_test <- glht(res_aov2,
                  linfct = mcp(choice= "Tukey")
)

summary(post_test)
 

# afex library tests ---
library(afex)
dat_clean$participant<-as.factor(dat_clean$participant) #Changes variable "Subject" from an integer to a factor.
dat_clean$Partner<-factor(dat_clean$Partner) #Changes variable "partner" from an integer to a factor.
dat_clean$choice<-factor(dat_clean$choice) #Changes variable "choice" from an integer to a factor.
Within.aov.1 <- aov_car(sync_value ~ Partner*choice + Error(participant/Partner*choice), data=dat_clean)

library(afex)
dat_stats = dat_clean %>%
  group_by(participant, Partner, which_block ) %>%
  summarize(ave_sync = mean(sync_mixed_Comp_Human))

dat_stats_2 = dat_preprocessed %>%
  group_by(participant, Partner, choice) %>%
  summarize(ave_sync = mean(sync_value, na.rm = T))

library(lme4)
library(lmerTest)



dat_stats = dat_clean %>%
 # filter(dat_clean$choice %in% c("comp_congruent", "human_congruent")) %>%
  group_by(participant, choice) %>%
  summarize(ave_sync = mean(sync_value))


lm = aov_car(ave_sync ~ choice+ Error(1/participant*choice), dat_stats)
summary(lm)
post_hoc = emmeans(lm, ~ choice)
post_hoc

dat_stats = dat_clean %>%
  # filter(dat_clean$choice %in% c("comp_congruent", "human_congruent")) %>%
  group_by(participant, Partner, CongruencyChoice) %>%
  summarize(ave_sync = mean(sync_value))


lm = aov_car(ave_sync ~ Partner*CongruencyChoice+ Error(1/participant*Partner*CongruencyChoice), dat_stats)
summary(lm)

library(rstatix)
# friedman test for dyad correctness, effect size and pairwise comparison
dat_stats = dat_clean %>%
  group_by(participant, Partner) %>%
  summarize(ave_Correctness = mean(CorrTuringDyad))
dat_stats = dat_stats %>%
  convert_as_factor(participant, Partner)
dat_stats$ave_Correctness = as.numeric(dat_stats$ave_Correctness)
dat_stats= ungroup(dat_stats)

res.fried <- dat_stats %>% friedman_test(ave_Correctness ~ Partner | participant)
res.fried
dat_stats %>% friedman_effsize(ave_Correctness ~ Partner | participant)


pwc <- dat_stats %>%
  wilcox_test(ave_Correctness ~ Partner, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Partner and CorrectedDyad
dat_stats = dat_clean %>%
  group_by(participant, Partner, CorrTuringDyad) %>%
  summarize(ave_sync = mean(sync_value))

lm2 = aov_car(ave_sync ~ Partner*CorrTuringDyad + Error(1/participant*Partner*CorrTuringDyad), dat_stats)
summary(lm2)

post_hoc = emmeans(lm2, ~ Partner*CorrTuringDyad)
post_hoc


library(emmeans)
post_hoc_lm1= emmeans(lm1, ~ Partner*which_block)

post_hoc_lm2 = emmeans(lm2, ~ Partner)


# mixed model
library(lme4)
library(lmerTest)

lm1 = lmer(sync_value~Partner*choice + 
             (1|participant), 
           dat_preprocessed)

lm2 = lmer(sync_value~Partner*choice + 
             (1|participant)+
             (1|trial_num), 
           dat_preprocessed)
anova(lm1, lm2)

lm3 = lmer(sync_value~Partner*choice + 
             (1|participant)+
             (1|trial_num) +
             (1|Block.thisN), 
           dat_preprocessed)
anova(lm1, lm2, lm3)
anova(lm3)

lm4 = lmer(sync_value~Partner*choice + 
             (1|participant)+
             (1+trial_num|participant) +
             (1|Block.thisN), 
           dat_preprocessed)
anova(lm1, lm2, lm3, lm4)
anova(lm4)

lm5 = lmer(sync_value~Partner*choice + 
             (1|participant)+
             (1+trial_num|participant) +
             (1+Block.thisN|participant),
             dat_preprocessed)
anova(lm1, lm2, lm3, lm4, lm5)
anova(lm4)

lm5 = lmer(sync_value~Partner*choice + 
             (1|participant)+
             (1+trial_num|participant/Partner) +
             (1+Block.thisN|participant/Partner),
           dat_preprocessed)
anova(lm1, lm2, lm3, lm4, lm5)
anova(lm5)


dat_preprocessed %>%
  ggplot(aes(Partner, sync_value))+
  stat_summary(fun.data = mean_se, geom = "point", size = 3)+
  #geom_jitter()+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1)

dat_preprocessed %>%
  ggplot(aes(choice, sync_value))+
  stat_summary(fun.data = mean_se, geom = "point", size = 3)+
  #geom_jitter()+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1)

# 
(dat_preprocessed %>% group_by(Partner) %>%
    summarize(mean = mean(sync_value)))
(dat_preprocessed %>% group_by(choice) %>%
    summarize(mean = mean(sync_value)))


lm_rating = lmer(sync_value~Partner*choice+closeness_diff + 
             (1|participant)+
               (1+trial_num|participant/Partner) +
               (1+Block.thisN|participant/Partner),
           dat_preprocessed)
anova(lm_rating)

dat_preprocessed %>%
  mutate(close_split_avg = ifelse(closeness_avg >= mean(closeness_avg), "close", "far"),
         close_split_diff = ifelse(closeness_diff >= mean(closeness_diff), "far", "close")) %>%
  ggplot(aes(Partner, sync_value, color = choice))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.8),
               fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.8))+
  theme_bw()+
  facet_wrap(~ close_split_diff)

dat_preprocessed %>%
  ggplot(aes(Partner, sync_value, color = choice))+
stat_summary(fun.data = mean_se, geom = "errorbar", 
             width = .1, position = position_dodge(.8),
             fun.args = list(mult = 1.96))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.8))+
  theme_bw()+
  facet_wrap(~ which_block)
