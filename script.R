library(dmetar)
library(meta)
library(esc)
library(tidyverse)
library(openxlsx)
library(metafor)

means <- c(262, 389.5)            # Means of the groups
sds <- c(63.4, 66.6)              # Standard deviations of the groups
sample_sizes <- c(20, 20) 
overall_mean <- sum(means * sample_sizes) / sum(sample_sizes)
n_groups <- length(means)
pooled_variance <- sum((sample_sizes - 1) * sds^2) + sum(sample_sizes * (means - overall_mean)^2)
pooled_variance <- pooled_variance / (sum(sample_sizes) - 1)
overall_sd <- sqrt(pooled_variance)

remove(means)
remove(sds)
remove(sample_sizes)
remove(overall_mean)
remove(pooled_variance)
remove(overall_sd)
remove(n_groups)

ttcomplete <- read.xlsx("ttcomplete.xlsx")
ttsensory <- read.xlsx("ttsensory.xlsx")
completesensory <- read.xlsx("completesensory.xlsx")
ttmotor <- read.xlsx("ttmotor.xlsx")
compeltemotor <- read.xlsx("completemotor.xlsx")
sensoryduration <- read.xlsx("sensoryduration.xlsx")
motorduration <- read.xlsx("motorduration.xlsx")
analgesicduration <- read.xlsx("analgesicduration.xlsx")
failure <- read.xlsx("failure.xlsx")


m.complete.md <- metacont(n.e = ne,
                          mean.e = meane,
                          sd.e = sde,
                          n.c = nc,
                          mean.c = meanc,
                          sd.c = sdc,
                          studlab = study,
                          data = ttcomplete,
                          sm = "MD",
                          fixed = FALSE,
                          random = TRUE,
                          method.tau = "REML",
                          method.random.ci = "HK",
                          title = "Time to Block")

m.ttsensory.md <- metacont(n.e = ne,
                           mean.e = meane,
                           sd.e = sde,
                           n.c = nc,
                           mean.c = meanc,
                           sd.c = sdc,
                           studlab = study,
                           data = ttsensory,
                           sm = "MD",
                           fixed = FALSE,
                           random = TRUE,
                           method.tau = "REML",
                           method.random.ci = "HK",
                           title = "Time to Sensory Block")

m.completesensory.md <- metacont(n.e = ne,
                                 mean.e = meane,
                                 sd.e = sde,
                                 n.c = nc,
                                 mean.c = meanc,
                                 sd.c = sdc,
                                 studlab = study,
                                 data = completesensory,
                                 sm = "MD",
                                 fixed = FALSE,
                                 random = TRUE,
                                 method.tau = "REML",
                                 method.random.ci = "HK",
                                 title = "Time to Complete Sensory Block")

m.ttmotor.md <- metacont(n.e = ne,
                         mean.e = meane,
                         sd.e = sde,
                         n.c = nc,
                         mean.c = meanc,
                         sd.c = sdc,
                         studlab = study,
                         data = ttmotor,
                         sm = "SMD",
                         fixed = FALSE,
                         random = TRUE,
                         method.tau = "REML",
                         method.random.ci = "HK",
                         title = "Time to Motor Block")

m.completemotor.md <- metacont(n.e = ne,
                               mean.e = meane,
                               sd.e = sde,
                               n.c = nc,
                               mean.c = meanc,
                               sd.c = sdc,
                               studlab = study,
                               data = compeltemotor,
                               sm = "MD",
                               fixed = FALSE,
                               random = TRUE,
                               method.tau = "REML",
                               method.random.ci = "HK",
                               title = "Time to Complete Motor Block")

m.sensoryduration.md <- metacont(n.e = ne,
                                 mean.e = meane,
                                 sd.e = sde,
                                 n.c = nc,
                                 mean.c = meanc,
                                 sd.c = sdc,
                                 studlab = study,
                                 data = sensoryduration,
                                 sm = "MD",
                                 fixed = FALSE,
                                 random = TRUE,
                                 method.tau = "REML",
                                 method.random.ci = "HK",
                                 title = "Duration of Motor Block")

m.motorduration.md <- metacont(n.e = ne,
                               mean.e = meane,
                               sd.e = sde,
                               n.c = nc,
                               mean.c = meanc,
                               sd.c = sdc,
                               studlab = study,
                               data = motorduration,
                               sm = "MD",
                               method.smd = "Hedges",
                               fixed = FALSE,
                               random = TRUE,
                               method.tau = "REML",
                               method.random.ci = "HK",
                               title = "Duration of Motor Block")

m.analgesicduration.md <- metacont(n.e = ne,
                                   mean.e = meane,
                                   sd.e = sde,
                                   n.c = nc,
                                   mean.c = meanc,
                                   sd.c = sdc,
                                   studlab = study,
                                   data = analgesicduration,
                                   sm = "MD",
                                   method.smd = "Hedges",
                                   fixed = FALSE,
                                   random = TRUE,
                                   method.tau = "REML",
                                   method.random.ci = "HK",
                                   title = "Duration of Analgesia")

meta::forest(m.complete.md, 
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed Local",
             label.c = "Long Acting",
             label.left = "Mixture Faster",
             label.right = "Long Acting Faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1,
             leftcols = c("studlab"),
             leftlabs = c("Study"))

meta::forest(m.ttsensory.md, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed Local",
             label.c = "Long Acting",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1
)

meta::forest(m.ttmotor.md, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed Local",
             label.c = "Long Acting",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)

meta::forest(m.motorduration.md, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed",
             label.c = "Long Acting",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)

meta::forest(m.analgesicduration.md, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed",
             label.c = "Long Acting",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)

m.complete.md.rma <- rma(yi = m.complete.md$TE, sei = m.complete.smd$seTE, method = m.complete.smd$method.tau, test = "knha")
m.complete.md.gosh <- gosh (m.complete.md.rma)
