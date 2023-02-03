

##Packages###

#Packages to be used
packages<-c("meta","bayesmeta","ggplot2","here","readxl",
            "tidyverse","grid","ggridges","ggthemes","extrafont","gridExtra",
            "ggridges","knitr","coda","rgl","metafor", "brms",
            "tidybayes","modelr","extraDistr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))



dir.create(here("Results"))

dir.create(here("Figures"))


source(here("Script","Mixed_effects.R"))

