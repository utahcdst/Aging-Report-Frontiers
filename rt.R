library(emmeans)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(Hmisc)
library(lme4)
library(stringr)

data <- read_csv("../compiled data/drt.csv") %>%
  filter(!is.na(task), bttn_prior == "T" | task == "Single" | task == "nBack"| task != "SuRT") %>%
  mutate(mode = str_replace(mode, "Center Consol", "Center Console")) %>%
  mutate(Task    = factor(task),
         Cohort  = factor(Cohort),
         Vehicle = factor(Vehicle),
         SUB     = factor(sub)) %>%
  filter(hit == "TRUE") %>%
  mutate(Mode = if_else(Task == "nBack", "nBack", if_else(Task == "Single", "Single", if_else(Task == "SuRT", "SuRT", mode)))) %>%
  select(sub, Vehicle, Task, Mode, Cohort, rt) %>%
  mutate(Task = fct_relevel(Task, c("Single", "Audio Entertainment", "Calling and Dialing", "Text Messaging", "Navigation Entry", "nBack", "SuRT"))) %>%
  mutate(Mode = fct_relevel(Mode, c("Single", "Auditory Vocal", "Center Console", "Center Stack", "nBack", "SuRT"))) %>%
  group_by(sub, Vehicle, Task, Mode, Cohort) %>%
  summarise(`Reaction Time` = mean(rt))

#----Cohort--------------------------------------------------------------------------------------------------------------------
# Cohort
subject  <- lmer(`Reaction Time` ~          (1|sub), data = data, REML = FALSE)
cohort   <- lmer(`Reaction Time` ~ Cohort + (1|sub), data = data, REML = FALSE)

anova(subject, cohort)

#----Task--------------------------------------------------------------------------------------------------------------------
# Cohort / Task
subject       <- lmer(`Reaction Time` ~                 (1|sub), data = data, REML = FALSE)
cohort        <- lmer(`Reaction Time` ~        Cohort + (1|sub), data = data, REML = FALSE)
task          <- lmer(`Reaction Time` ~ Task          + (1|sub), data = data, REML = FALSE)
task_cohort   <- lmer(`Reaction Time` ~ Task + Cohort + (1|sub), data = data, REML = FALSE)
task_x_cohort <- lmer(`Reaction Time` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)

anova(task         , subject)
anova(task_cohort  , cohort)
anova(task_x_cohort, task_cohort)

  # Pairwise - Audio Entertainment Reference
  summary(task_x_cohort)
  
  # Pairwise - Calling and Dialing Reference 
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Calling and Dialing"))
  task_x_cohort <- lmer(`Reaction Time` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - Text Messaging Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Text Messaging"))
  task_x_cohort <- lmer(`Reaction Time` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - Navigation Entry Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Navigation Entry"))
  task_x_cohort <- lmer(`Reaction Time` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - Single Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "Single"))
  task_x_cohort <- lmer(`Reaction Time` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - nBack Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "nBack"))
  task_x_cohort <- lmer(`Reaction Time` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)
  
  # Pairwise - SuRT Reference
  data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, "SuRT"))
  task_x_cohort <- lmer(`Reaction Time` ~ Task * Cohort + (1|sub), data = data, REML = FALSE)
  summary(task_x_cohort)

# Plot
data <- data %>% ungroup(Task) %>%  mutate(Task = fct_relevel(Task, c("Single", "Audio Entertainment", "Calling and Dialing", "Text Messaging", "Navigation Entry", "nBack","SuRT")))
ggplot(data, aes(Task, `Reaction Time`)) +
  geom_boxplot(aes(fill=Cohort)) +
  theme_bw() +
  ylim(0,1750) +
  geom_vline(xintercept = c(1.5, 5.5), size=.5, linetype="dashed") +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title="", x="", y="Milliseconds") +
  ggsave("Charts/rt_task.jpg", height = 4, width = 5, dpi = 300)

#----Mode--------------------------------------------------------------------------------------------------------------------
# Cohort / Mode
subject       <- lmer(`Reaction Time` ~                 (1|sub), data = data, REML = FALSE)
cohort        <- lmer(`Reaction Time` ~        Cohort + (1|sub), data = data, REML = FALSE)
mode          <- lmer(`Reaction Time` ~ Mode          + (1|sub), data = data, REML = FALSE)
mode_cohort   <- lmer(`Reaction Time` ~ Mode + Cohort + (1|sub), data = data, REML = FALSE)
mode_x_cohort <- lmer(`Reaction Time` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)

anova(mode         , subject)
anova(mode_cohort  , cohort)
anova(mode_x_cohort, mode_cohort)

# Pairwise - Auditory Vocal Reference
summary(mode_x_cohort)

# Pairwise - Center Console Reference 
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Center Console"))
mode_x_cohort <- lmer(`Reaction Time` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - Center Stack Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Center Stack"))
mode_x_cohort <- lmer(`Reaction Time` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - Single Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "Single"))
mode_x_cohort <- lmer(`Reaction Time` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - nBack Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "nBack"))
mode_x_cohort <- lmer(`Reaction Time` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Pairwise - SuRT Reference
data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Mode, "SuRT"))
mode_x_cohort <- lmer(`Reaction Time` ~ Mode * Cohort + (1|sub), data = data, REML = FALSE)
summary(mode_x_cohort)

# Plot

data <- data %>% ungroup(Mode) %>%  mutate(Mode = fct_relevel(Task, c("Single", "Audio Entertainment", "Calling and Dialing", "Text Messaging", "Navigation Entry", "nBack","SuRT")))
ggplot(data, aes(Mode, `Reaction Time`)) +
  geom_boxplot(aes(fill=Cohort)) +
  theme_bw() +
  ylim(0,1750) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  geom_vline(xintercept = c(1.5, 5.5), size=.5, linetype="dashed") +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title="", x="", y="Milliseconds") +
  ggsave("Charts/rt_mode.jpg", height = 4, width = 4.5, dpi = 300)

#----Vehicle--------------------------------------------------------------------------------------------------------------------
# Cohort / Vehicle
subject          <- lmer(`Reaction Time` ~                    (1|sub), data = data, REML = FALSE)
cohort           <- lmer(`Reaction Time` ~           Cohort + (1|sub), data = data, REML = FALSE)
vehicle          <- lmer(`Reaction Time` ~ Vehicle          + (1|sub), data = data, REML = FALSE)
vehicle_cohort   <- lmer(`Reaction Time` ~ Vehicle + Cohort + (1|sub), data = data, REML = FALSE)
vehicle_x_cohort <- lmer(`Reaction Time` ~ Vehicle * Cohort + (1|sub), data = data, REML = FALSE)

anova(vehicle         , subject)
anova(vehicle_cohort  , cohort)
anova(vehicle_x_cohort, vehicle_cohort) 

  # Pairwise - Audi Reference
  summary(vehicle_x_cohort)
  
  # Pairwise - Cadillac Reference 
  data <- data %>% ungroup(Vehicle) %>%  mutate(Vehicle = fct_relevel(Vehicle, "Cadillac CT6"))
  vehicle_x_cohort <- lmer(`Reaction Time` ~ Vehicle * Cohort + (1|sub), data = data, REML = FALSE)
  summary(vehicle_x_cohort)
  
  # Pairwise - Lincoln Reference
  data <- data %>% ungroup(Vehicle) %>%  mutate(Vehicle = fct_relevel(Vehicle, "Lincoln Navigator"))
  vehicle_x_cohort <- lmer(`Reaction Time` ~ Vehicle * Cohort + (1|sub), data = data, REML = FALSE)
  summary(vehicle_x_cohort)
  
  # Pairwise - Mazda Reference
  data <- data %>% ungroup(Vehicle) %>%  mutate(Vehicle = fct_relevel(Vehicle, "Mazda CX5"))
  vehicle_x_cohort <- lmer(`Reaction Time` ~ Vehicle * Cohort + (1|sub), data = data, REML = FALSE)
  summary(vehicle_x_cohort)
  
  # Pairwise - Nissan Reference
  data <- data %>% ungroup(Vehicle) %>%  mutate(Vehicle = fct_relevel(Vehicle, "Nissan Pathfinder"))
  vehicle_x_cohort <- lmer(`Reaction Time` ~ Vehicle * Cohort + (1|sub), data = data, REML = FALSE)
  summary(vehicle_x_cohort)
  
  # Pairwise - Volvo Reference
  data <- data %>% ungroup(Vehicle) %>%  mutate(Vehicle = fct_relevel(Vehicle, "Volvo XC90"))
  vehicle_x_cohort <- lmer(`Reaction Time` ~ Vehicle * Cohort + (1|sub), data = data, REML = FALSE)
  summary(vehicle_x_cohort)

#Plot
data <- data %>% ungroup(Vehicle) %>%  mutate(Vehicle = fct_relevel(Vehicle, c("Audi A6", "Cadillac CT6", "Lincoln Navigator", "Mazda CX5", "Nissan Pathfinder", "Volvo XC90")))
  
ggplot(data, aes(Vehicle, `Reaction Time`)) +
  geom_boxplot(aes(fill=Cohort)) +
  theme_bw() +
  ylim(0,1750) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title="", x="", y="Milliseconds") +
  ggsave("Charts/rt_vehicle.jpg", height = 4, width = 4, dpi = 300)

#----Means--------------------------------------------------------------------------------------------------------------------
# Means and SD

    # Cohort
    data %>% group_by(Cohort)     %>% summarise(m = mean(`Reaction Time`), sd = sd(`Reaction Time`))
    # Task
    data %>% group_by(Task)       %>% summarise(m = mean(`Reaction Time`), sd = sd(`Reaction Time`))
    # Mode
    data %>% group_by(Mode)       %>% summarise(m = mean(`Reaction Time`), sd = sd(`Reaction Time`))
    # Vehicle
    data %>% group_by(Vehicle)    %>% summarise(m = mean(`Reaction Time`), sd = sd(`Reaction Time`))
    
    # Task x Cohort
    data %>% group_by(Task,    Cohort) %>% summarise(m = mean(`Reaction Time`), sd = sd(`Reaction Time`))
    # Mode x Cohort
    data %>% group_by(Mode,    Cohort) %>% summarise(m = mean(`Reaction Time`), sd = sd(`Reaction Time`))
    # Vehicle x Cohort
    data %>% group_by(Vehicle, Cohort) %>% summarise(m = mean(`Reaction Time`), sd = sd(`Reaction Time`))
    
#----Full Factorial--------------------------------------------------------------------------------------------------------------------
    
    # Analyses
    ctm_x <- lmer(`Reaction Time` ~ Cohort + Task + Mode + Cohort:Task + Task:Mode + Mode:Cohort + (1|sub), data = data, REML = FALSE)
    ctm   <- lmer(`Reaction Time` ~ Cohort * Task * Mode + (1|sub), data = data, REML = FALSE)
    anova(ctm, ctm_x)
    
    ctv_x <- lmer(`Reaction Time` ~ Cohort + Task + Vehicle + Cohort:Task + Task:Vehicle + Cohort:Vehicle + (1|sub), data = data, REML = FALSE)
    ctv   <- lmer(`Reaction Time` ~ Cohort * Task * Vehicle + (1|sub), data = data, REML = FALSE)
    anova(ctv, ctv_x)
    
    cmv_x <- lmer(`Reaction Time` ~ Cohort + Mode + Vehicle + Cohort:Mode + Mode:Vehicle + Cohort:Vehicle + (1|sub), data = data, REML = FALSE)
    cmv   <- lmer(`Reaction Time` ~ Cohort * Mode * Vehicle + (1|sub), data = data, REML = FALSE)
    anova(cmv, cmv_x)
    
    vtm_x <- lmer(`Reaction Time` ~ Vehicle + Task + Mode + Vehicle:Task + Task:Mode + Mode:Vehicle + (1|sub), data = data, REML = FALSE)
    vtm   <- lmer(`Reaction Time` ~ Vehicle * Task * Mode + (1|sub), data = data, REML = FALSE)
    anova(vtm, vtm_x)
    
    
    ctmv_x <- lmer(`Reaction Time` ~ Cohort + Task + Mode + Vehicle
                   + Cohort:Task + Task:Mode + Mode:Vehicle + Cohort:Mode + Cohort:Vehicle + Vehicle:Task
                   + (1|sub), data = data, REML = FALSE)
    ctmv   <- lmer(`Reaction Time` ~ Cohort * Task * Mode * Vehicle + (1|sub), data = data, REML = FALSE)
    anova(ctmv, ctmv_x)  
    
  