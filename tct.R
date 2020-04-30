library(emmeans)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(Hmisc)
library(lme4)
library(stringr)

tct <- list()
data <- read_csv("../compiled data/task.csv") %>%
  filter(!is.na(task), task != "Single", task != "SuRT", task != "nBack") %>%
  mutate(mode = str_replace(mode, "Center Consol", "Center Console")) %>%
  mutate(make    = factor(make),
         model   = factor(model),
         year    = factor(year),
         Task    = factor(task),
         Mode    = factor(mode),
         Cohort  = factor(Cohort),
         Vehicle = factor(Vehicle),
         Seconds = taskMillis/1000,
         sqrtTCT = sqrt(taskMillis)) %>%
  mutate(Task = fct_relevel(Task, c("Audio Entertainment", "Calling and Dialing", "Text Messaging", "Navigation Entry"))) %>%
  group_by(sub, Task, Mode, Vehicle, Cohort) %>%
  summarise(Seconds = mean(Seconds, na.rm=T))

#----Cohort--------------------------------------------------------------------------------------------------------------------
# Cohort
subject  <- lmer(Seconds ~          (1|sub), data = data, REML = FALSE)
cohort   <- lmer(Seconds ~ Cohort + (1|sub), data = data, REML = FALSE)

anova(subject, cohort)

#----Task--------------------------------------------------------------------------------------------------------------------
# Cohort / Task
subject       <- lmer(Seconds ~                 (1|sub), data = data, REML = FALSE)
cohort        <- lmer(Seconds ~        Cohort + (1|sub), data = data, REML = FALSE)
task          <- lmer(Seconds ~ Task          + (1|sub), data = data, REML = FALSE)
task_cohort   <- lmer(Seconds ~ Task + Cohort + (1|sub), data = data, REML = FALSE)
task_x_cohort <- lmer(Seconds ~ Task * Cohort + (1|sub), data = data, REML = FALSE)

anova(task         , subject)
anova(task_cohort  , cohort)
anova(task_x_cohort, task_cohort)

    ##
    # Pairwise - Main Effects of Task
    # Audio Entertainment Reference
    summary(task)
    
    # Pairwise - Calling and Dialing Reference 
    data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Calling and Dialing"))
    task <- lmer(Seconds ~ Task + (1|sub), data = data, REML = FALSE)
    summary(task)
    
    # Pairwise - Text Messaging Reference
    data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Text Messaging"))
    task <- lmer(Seconds ~ Task + (1|sub), data = data, REML = FALSE)
    summary(task)
    
    # Pairwise - Navigation Entry Reference
    data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Navigation Entry"))
    task <- lmer(Seconds ~ Task + (1|sub), data = data, REML = FALSE)
    summary(task)
    
  
    ##
    # Pairwise - Cohort x Task Interaction
    # Audio Entertainment Reference
    summary(task_x_cohort)
    
    # Pairwise - Calling and Dialing Reference 
    data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Calling and Dialing"))
    task_x_cohort <- lmer(Seconds ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
    summary(task_x_cohort)
    
    # Pairwise - Text Messaging Reference
    data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Text Messaging"))
    task_x_cohort <- lmer(Seconds ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
    summary(task_x_cohort)
    
    # Pairwise - Navigation Entry Reference
    data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Navigation Entry"))
    task_x_cohort <- lmer(Seconds ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
    summary(task_x_cohort)
    
# Plot
data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, c("Audio Entertainment", "Calling and Dialing", "Text Messaging", "Navigation Entry")))
ggplot(data, aes(Task, Seconds)) +
  geom_boxplot(aes(fill=Cohort)) +
  geom_hline(yintercept = 24, linetype="dashed", color="red") + 
  theme_bw() +
  ylim(0,75) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title="", x="") +
  ggsave("Charts/tot_task.jpg", height = 4, width = 3.5, dpi = 300)
    
#----Mode--------------------------------------------------------------------------------------------------------------------
# Cohort / Mode
subject       <- lmer(Seconds ~                 (1|sub), data = data, REML = FALSE)
cohort        <- lmer(Seconds ~        Cohort + (1|sub), data = data, REML = FALSE)
mode          <- lmer(Seconds ~ Mode          + (1|sub), data = data, REML = FALSE)
mode_cohort   <- lmer(Seconds ~ Mode + Cohort + (1|sub), data = data, REML = FALSE)
mode_x_cohort <- lmer(Seconds ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)

anova(mode         , subject)
anova(mode_cohort  , cohort)
anova(mode_x_cohort, mode_cohort)
    
    ## Main effect of mode
    # Pairwise - Auditory Vocal Reference
    summary(mode)
    
    # Pairwise - Center Console Reference 
    data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Center Console"))
    mode <- lmer(Seconds ~ Mode + (1|sub), data = data, REML = FALSE)
    summary(mode)
    
    # Pairwise - Center Stack Reference
    data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Center Stack"))
    mode <- lmer(Seconds ~ Mode + (1|sub), data = data, REML = FALSE)
    summary(mode)
    
    ## Interaction of age and mode
    # Pairwise - Auditory Vocal Reference
    summary(mode_x_cohort)
    
    # Pairwise - Center Console Reference 
    data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Center Console"))
    mode_x_cohort <- lmer(Seconds ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
    summary(mode_x_cohort)
    
    # Pairwise - Center Stack Reference
    data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Center Stack"))
    mode_x_cohort <- lmer(Seconds ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
    summary(mode_x_cohort)

# Plot

data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, c("Auditory Vocal", "Center Console", "Center Stack")))
ggplot(data, aes(Mode, Seconds)) +
  geom_boxplot(aes(fill=Cohort)) +
  geom_hline(yintercept = 24, linetype="dashed", color="red") + 
  theme_bw() +
  ylim(0,75) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title="", x="", y="Seconds") +
  ggsave("Charts/tot_mode.jpg", height = 4, width = 3, dpi = 300)

#----Means--------------------------------------------------------------------------------------------------------------------
# Means and SD

    # Cohort
    data %>% group_by(Cohort)     %>% summarise(m = mean(Seconds), sd = sd(Seconds))
    # Task
    data %>% group_by(Task)       %>% summarise(m = mean(Seconds), sd = sd(Seconds))
    # Mode
    data %>% group_by(Mode)       %>% summarise(m = mean(Seconds), sd = sd(Seconds))
    # Task x Cohort
    data %>% group_by(Task,    Cohort) %>% summarise(m = mean(Seconds), sd = sd(Seconds))
    # Mode x Cohort
    data %>% group_by(Mode,    Cohort) %>% summarise(m = mean(Seconds), sd = sd(Seconds))
  
#----3 way interactions--------------------------------------------------------------------------------------------------------------------
# Means and SD
data <- data %>%
  filter(Task != "SuRT", Task != "nBack", Task != "Single") %>%
  mutate(Task =    fct_relevel(Task, c("Audio Entertainment", "Calling and Dialing", "Text Messaging", "Navigation Entry")),
         Mode =    fct_relevel(Mode, c("Auditory Vocal", "Center Console", "Center Stack")),
         Vehicle = fct_relevel(Vehicle, c("Audi A6", "Cadillac CT6", "Lincoln Navigator", "Mazda CX5", "Nissan Pathfinder", "Volvo XC90")))

    # Analyses
    
    
    ctmv_x <- lmer(Seconds ~ Cohort + Task + Mode
                   + Cohort:Task + Task:Mode + Cohort:Mode
                   + (1|sub), data = data, REML = FALSE)
    ctmv   <- lmer(Seconds ~ Cohort * Task * Mode + (1|sub), data = data, REML = FALSE)
    anova(ctmv, ctmv_x)  
    
    
##Task Completion Time

ggplot(data, aes(Task, Seconds)) +
  geom_boxplot(aes(fill=Cohort), alpha=.8) +
  theme_bw() + 
  ylim(10,50) + 
  facet_grid(~Mode)+
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_fill_manual(values = c("white", "grey")) +
  ggsave("Charts/tct.jpg", height = 4, width = 6, dpi = 300)

    