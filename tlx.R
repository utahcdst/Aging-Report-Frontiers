library(emmeans)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(Hmisc)
library(lme4)
library(stringr)

data <- read_csv("../compiled data/TLX.csv") %>%
  separate(task,c("Task", "Mode"), "_") %>%
  mutate(mode = str_replace(Mode, "Center Consol", "Center Console")) %>%
  mutate(Vehicle = str_replace(Vehicle, "057a 2018 Nissan Pathfinder SL", "Nissan Pathfinder"),
         Vehicle = str_replace(Vehicle, "057b 2018 Nissan Pathfinder SL", "Nissan Pathfinder"),
         Vehicle = str_replace(Vehicle, "058a 2018 Mazda CX-5 Grand.Touring", "Mazda CX5"),
         Vehicle = str_replace(Vehicle, "058b 2018 Mazda CX-5 Grand.Touring", "Mazda CX5"),
         Vehicle = str_replace(Vehicle, "059a 2018 Volvo XC90 Momentum", "Volvo XC90"),
         Vehicle = str_replace(Vehicle, "059b 2018 Volvo XC90 Momentum", "Volvo XC90"),
         Vehicle = str_replace(Vehicle, "060a 2018 Audi A6 Premium", "Audi A6"),
         Vehicle = str_replace(Vehicle, "060b 2018 Audi A6 Premium", "Audi A6"),
         Vehicle = str_replace(Vehicle, "061a 2018 Lincoln Navigator Select L", "Lincoln Navigator"),
         Vehicle = str_replace(Vehicle, "061b 2018 Lincoln Navigator Select L", "Lincoln Navigator"),
         Vehicle = str_replace(Vehicle, "062a 2018 Cadillac CT6 Premium Luxury", "Cadillac CT6"),
         Vehicle = str_replace(Vehicle, "062b 2018 Cadillac CT6 Premium Luxury", "Cadillac CT6"),
         
         Cohort  = if_else(as.integer(Sub) < 500, "Age:21-36", "Age:55-75"),
         Task    = if_else(Task == "SMSRS", "Text Messaging",
                           if_else(Task == "SMSS",  "Text Messaging",
                                   if_else(Task == "SMSR",  "Text Messaging",
                                           if_else(Task == "Audio", "Audio Entertainment",
                                                   if_else(Task == "Nav",   "Navigation Entry",
                                                           if_else(Task == "CD",    "Calling and Dialing", Task)))))),
         Mode    = if_else(Mode == "V",     "Auditory Vocal",
                           if_else(Mode == "CS",    "Center Stack",
                                   if_else(Mode == "TS",    "Center Stack",
                                           if_else(Mode == "CC",    "Center Consol",
                                                   if_else(Mode == "RW",    "Center Consol", Mode))))),
         Mode = if_else(Task == "SuRT", "SuRT", if_else(Task == "Single", "Single", if_else(Task == "nBack", "nBack", as.character(Mode)))),
         Task    = factor(Task),
         Mode    = factor(Mode),
         Cohort  = factor(Cohort)) %>%
  mutate(Task = fct_relevel(Task, c("Single", "Audio Entertainment", "Calling and Dialing", "Text Messaging", "Navigation Entry", "nBack"))) %>%
  mutate(Mode = fct_relevel(Mode, c("Single", "Auditory Vocal", "Center Consol", "Center Stack", "nBack"))) %>%
  rowwise() %>%
  mutate(TLX = sum(Mental, Physical, Hurried, Work, Insecure)/5) %>%
  filter(!is.na(Cohort))

#----Cohort--------------------------------------------------------------------------------------------------------------------
# Cohort
Subject  <- lmer(TLX ~          (1|Sub), data = data, REML = FALSE)
cohort   <- lmer(TLX ~ Cohort + (1|Sub), data = data, REML = FALSE)

anova(Subject, cohort)

#----Task--------------------------------------------------------------------------------------------------------------------
# Cohort / Task
Subject       <- lmer(TLX ~                 (1|Sub), data = data, REML = FALSE)
cohort        <- lmer(TLX ~        Cohort + (1|Sub), data = data, REML = FALSE)
task          <- lmer(TLX ~ Task          + (1|Sub), data = data, REML = FALSE)
task_cohort   <- lmer(TLX ~ Task + Cohort + (1|Sub), data = data, REML = FALSE)
task_x_cohort <- lmer(TLX ~ Task * Cohort + (1|Sub), data = data, REML = FALSE)

anova(task         , Subject)
anova(task_cohort  , cohort)
anova(task_x_cohort, task_cohort)



  # Pairwise - Audio Entertainment Reference
  summary(task_x_cohort)
  
  # Pairwise - Calling and Dialing Reference 
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Calling and Dialing"))
  task_x_cohort <- lmer(TLX ~ Task * Cohort + (1|Sub), data = data, REML = FALSE)
  
  # Pairwise - Text Messaging Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Text Messaging"))
  task_x_cohort <- lmer(TLX ~ Task * Cohort + (1|Sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - Navigation Entry Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Navigation Entry"))
  task_x_cohort <- lmer(TLX ~ Task * Cohort + (1|Sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - Single Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Single"))
  task_x_cohort <- lmer(TLX ~ Task * Cohort + (1|Sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - nBack Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "nBack"))
  task_x_cohort <- lmer(TLX ~ Task * Cohort + (1|Sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - SuRT Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "SuRT"))
  task_x_cohort <- lmer(TLX ~ Task * Cohort + (1|Sub), data = data, REML = FALSE)
  summary(task_x_cohort)

# Plot
data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, c("Single", "Audio Entertainment", "Calling and Dialing", "Text Messaging", "Navigation Entry", "nBack","SuRT")))
ggplot(data, aes(Task, TLX)) +
  geom_boxplot(aes(fill=Cohort)) +
  theme_bw() +
  ylim(0,21) +
  geom_vline(xintercept = c(1.5, 5.5), size=.5, linetype="dashed") +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title="", x="", y="TLX") +
  ggsave("Charts/tlx_task.jpg", height = 4, width = 5, dpi = 300)
#----Mode--------------------------------------------------------------------------------------------------------------------
# Cohort / Mode
Subject       <- lmer(TLX ~                 (1|Sub), data = data, REML = FALSE)
cohort        <- lmer(TLX ~        Cohort + (1|Sub), data = data, REML = FALSE)
mode          <- lmer(TLX ~ Mode          + (1|Sub), data = data, REML = FALSE)
mode_cohort   <- lmer(TLX ~ Mode + Cohort + (1|Sub), data = data, REML = FALSE)
mode_x_cohort <- lmer(TLX ~ Mode * Cohort + (1|Sub), data = data, REML = FALSE)

anova(mode         , Subject)
anova(mode_cohort  , cohort)
anova(mode_x_cohort, mode_cohort)

# Pairwise - Auditory Vocal Reference
summary(mode_x_cohort)

# Pairwise - Center Console Reference 
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Center Console"))
mode_x_cohort <- lmer(TLX ~ Mode * Cohort + (1|Sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - Center Stack Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Center Stack"))
mode_x_cohort <- lmer(TLX ~ Mode * Cohort + (1|Sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - Single Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Single"))
mode_x_cohort <- lmer(TLX ~ Mode * Cohort + (1|Sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - nBack Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "nBack"))
mode_x_cohort <- lmer(TLX ~ Mode * Cohort + (1|Sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - SuRT Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "SuRT"))
mode_x_cohort <- lmer(TLX ~ Mode * Cohort + (1|Sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Plot

data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, c("Single", "Auditory Vocal", "Center Stack", "Center Console", "nBack","SuRT")))
ggplot(data, aes(Mode, TLX)) +
  geom_boxplot(aes(fill=Cohort)) +
  theme_bw() +
  ylim(0,21) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  geom_vline(xintercept = c(1.5, 4.5), size=.5, linetype="dashed") +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title="", x="", y="TLX") +
  ggsave("Charts/tlx_mode.jpg", height = 4, width = 4.5, dpi = 300)


#----Means--------------------------------------------------------------------------------------------------------------------
# Means and SD

# Cohort
data %>% group_by(Cohort)     %>% summarise(m = mean(TLX), sd = sd(TLX))
# Task
data %>% group_by(Task)       %>% summarise(m = mean(TLX), sd = sd(TLX))
# Mode
data %>% group_by(Mode)       %>% summarise(m = mean(TLX), sd = sd(TLX))
# Vehicle
data %>% group_by(Vehicle)    %>% summarise(m = mean(TLX), sd = sd(TLX))

# Task x Cohort
data %>% group_by(Task,    Cohort) %>% summarise(m = mean(TLX), sd = sd(TLX))
# Mode x Cohort
data %>% group_by(Mode,    Cohort) %>% summarise(m = mean(TLX), sd = sd(TLX))
# Vehicle x Cohort
data %>% group_by(Vehicle, Cohort) %>% summarise(m = mean(TLX), sd = sd(TLX))

#----Full Factorial--------------------------------------------------------------------------------------------------------------------
# Analyses


ctmv_x <- lmer(TLX ~ Cohort + Task + Mode 
               + Cohort:Task + Task:Mode + Cohort:Mode
               + (1|Sub), data = data, REML = FALSE)
ctmv   <- lmer(TLX ~ Cohort * Task * Mode + (1|Sub), data = data, REML = FALSE)
anova(ctmv, ctmv_x)  

data2 <- data %>%
  filter(Task != "nBack", Task != "SuRT", Task != "Single")

ggplot(data2, aes(Task, TLX)) +
  geom_boxplot(aes(fill=Cohort), alpha=.8) +
  theme_bw() + 
  #ylim(10, 2050) + 
  facet_grid(~Mode)+
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_fill_manual(values = c("white", "grey"))  +
  ggsave("Charts/tlx.jpg", height = 4, width = 6, dpi = 300)
