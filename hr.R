library(emmeans)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(Hmisc)
library(lme4)
library(stringr)

data <- read_csv("../compiled data/drt.csv") %>%
  filter(!is.na(task), stim == 1, bttn_prior == "T" |task == "SuRT" | task == "Single" | task == "nBack") %>%
  mutate(mode = str_replace(mode, "Center Consol", "Center Console")) %>%
  mutate(Cohort  = factor(Cohort),
         Vehicle = factor(Vehicle),
         sub     = factor(sub)) %>%
  select(sub, Vehicle, task, mode, Cohort, hit) %>%
  mutate(Mode = if_else(task == "SuRT", "SuRT", if_else(task == "Single", "Single", if_else(task == "nBack", "nBack", mode)))) %>%
  mutate(Mode = if_else(Mode == "Center Consol", "Center Consol", Mode)) %>%
  mutate(Task = fct_relevel(task, c("Single", "Audio Entertainment", "Calling and Dialing", "Text Messaging", "Navigation Entry", "nBack", "SuRT"))) %>%
  mutate(Mode = fct_relevel(Mode, c("Single", "Auditory Vocal", "Center Console", "Center Stack", "nBack", "SuRT"))) %>%
  group_by(sub, Vehicle, Task, Mode, Cohort) %>%
  summarise(`Hit Rate` = mean(hit))

#----Cohort--------------------------------------------------------------------------------------------------------------------
# Cohort
subject  <- lmer(`Hit Rate` ~          (1|sub), data = data, REML = FALSE)
cohort   <- lmer(`Hit Rate` ~ Cohort + (1|sub), data = data, REML = FALSE)

anova(subject, cohort)

#----Task--------------------------------------------------------------------------------------------------------------------
# Cohort / Task
subject       <- lmer(`Hit Rate` ~                 (1|sub), data = data, REML = FALSE)
cohort        <- lmer(`Hit Rate` ~        Cohort + (1|sub), data = data, REML = FALSE)
task          <- lmer(`Hit Rate` ~ Task          + (1|sub), data = data, REML = FALSE)
task_cohort   <- lmer(`Hit Rate` ~ Task + Cohort + (1|sub), data = data, REML = FALSE)
task_x_cohort <- lmer(`Hit Rate` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)

anova(task         , subject)
anova(task_cohort  , cohort)
anova(task_x_cohort, task_cohort)

  # Pairwise - Audio Entertainment Reference
  summary(task_x_cohort)
  
  # Pairwise - Calling and Dialing Reference 
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Calling and Dialing"))
  task_x_cohort <- lmer(`Hit Rate` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - Text Messaging Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Text Messaging"))
  task_x_cohort <- lmer(`Hit Rate` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - Navigation Entry Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Navigation Entry"))
  task_x_cohort <- lmer(`Hit Rate` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - Single Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Single"))
  task_x_cohort <- lmer(`Hit Rate` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - nBack Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "nBack"))
  task_x_cohort <- lmer(`Hit Rate` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - SuRT Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "SuRT"))
  task_x_cohort <- lmer(`Hit Rate` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)

# Plot
data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, c("Single", "Audio Entertainment", "Calling and Dialing", "Text Messaging", "Navigation Entry", "nBack","SuRT")))
ggplot(data, aes(Task, `Hit Rate`)) +
  geom_boxplot(aes(fill=Cohort)) +
  theme_bw() +
  ylim(0,1) +
  geom_vline(xintercept = c(1.5, 5.5), size=.5, linetype="dashed") +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title="", x="", y="Hit Rate") +
  ggsave("Charts/hr_task.jpg", height = 4, width = 5, dpi = 300)

#----Mode--------------------------------------------------------------------------------------------------------------------
# Cohort / Mode
subject       <- lmer(`Hit Rate` ~                 (1|sub), data = data, REML = FALSE)
cohort        <- lmer(`Hit Rate` ~        Cohort + (1|sub), data = data, REML = FALSE)
mode          <- lmer(`Hit Rate` ~ Mode          + (1|sub), data = data, REML = FALSE)
mode_cohort   <- lmer(`Hit Rate` ~ Mode + Cohort + (1|sub), data = data, REML = FALSE)
mode_x_cohort <- lmer(`Hit Rate` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)

anova(mode         , subject)
anova(mode_cohort  , cohort)
anova(mode_x_cohort, mode_cohort)

# Pairwise - Auditory Vocal Reference
summary(mode_x_cohort)

# Pairwise - Center Console Reference 
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Center Console"))
mode_x_cohort <- lmer(`Hit Rate` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - Center Stack Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Center Stack"))
mode_x_cohort <- lmer(`Hit Rate` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - Single Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Single"))
mode_x_cohort <- lmer(`Hit Rate` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - nBack Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "nBack"))
mode_x_cohort <- lmer(`Hit Rate` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - SuRT Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "SuRT"))
mode_x_cohort <- lmer(`Hit Rate` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Plot

data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, c("Single", "Auditory Vocal", "Center Stack", "Center Console", "nBack","SuRT")))
ggplot(data, aes(Mode, `Hit Rate`)) +
  geom_boxplot(aes(fill=Cohort)) +
  theme_bw() +
  ylim(0,1) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  geom_vline(xintercept = c(1.5, 4.5), size=.5, linetype="dashed") +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title="", x="", y="Hit Rate") +
  ggsave("Charts/hr_mode.jpg", height = 4, width = 4.5, dpi = 300)


#----Means--------------------------------------------------------------------------------------------------------------------
# Means and SD

# Cohort
data %>% group_by(Cohort)     %>% summarise(m = mean(`Hit Rate`), sd = sd(`Hit Rate`))
# Task
data %>% group_by(Task)       %>% summarise(m = mean(`Hit Rate`), sd = sd(`Hit Rate`))
# Mode
data %>% group_by(Mode)       %>% summarise(m = mean(`Hit Rate`), sd = sd(`Hit Rate`))
# Vehicle
data %>% group_by(Vehicle)    %>% summarise(m = mean(`Hit Rate`), sd = sd(`Hit Rate`))

# Task x Cohort
data %>% group_by(Task,    Cohort) %>% summarise(m = mean(`Hit Rate`), sd = sd(`Hit Rate`))
# Mode x Cohort
data %>% group_by(Mode,    Cohort) %>% summarise(m = mean(`Hit Rate`), sd = sd(`Hit Rate`))
# Vehicle x Cohort
data %>% group_by(Vehicle, Cohort) %>% summarise(m = mean(`Hit Rate`), sd = sd(`Hit Rate`))

#----Full Factorial--------------------------------------------------------------------------------------------------------------------
# Analyses


ctmv_x <- lmer(`Hit Rate` ~ Cohort + Task + Mode
               + Cohort:Task + Task:Mode  + Cohort:Mode
               + (1|sub), data = data, REML = FALSE)
ctmv   <- lmer(`Hit Rate` ~ Cohort * Task * Mode + (1|sub), data = data, REML = FALSE)
anova(ctmv, ctmv_x)    


data2 <- data %>%
  filter(Task != "nBack", Task != "SuRT", Task != "Single")


ggplot(data2, aes(Task, `Hit Rate`)) +
  geom_boxplot(aes(fill=Cohort), alpha=.8) +
  theme_bw() + 
  #ylim(10, 2050) + 
  facet_grid(~Mode)+
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_fill_manual(values = c("white", "grey")) +
  ggsave("Charts/hr.jpg", height = 4, width = 6, dpi = 300)
