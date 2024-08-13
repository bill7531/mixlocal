library(dmetar)
library(meta)
library(esc)
library(tidyverse)
library(openxlsx)
library(metafor)
library(ggplot2)
library(gridExtra)
library(fpc)
library(mclust)

means <- c(16.64, 16.92)            # Means of the groups
sds <- c(0.89, 0.51)              # Standard deviations of the groups
sample_sizes <- c(17, 40) 
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
ttcomplete.sen <- read.xlsx("ttcomplete-sen.xlsx")
ttcomplete.byla <- read.xlsx("ttcomplete.byla.xlsx")
ttcomplete.sen.byla <- read.xlsx("ttcomplete.sen.byla.xlsx")
ttsensory <- read.xlsx("ttsensory.xlsx")
completesensory <- read.xlsx("completesensory.xlsx")
ttmotor <- read.xlsx("ttmotor.xlsx")
compeltemotor <- read.xlsx("completemotor.xlsx")
sensoryduration <- read.xlsx("sensoryduration.xlsx")
sensoryduration.la <- read.xlsx("sensoryduration.la.xlsx")
motorduration <- read.xlsx("motorduration.xlsx")
analgesicduration <- read.xlsx("analgesicduration.xlsx")
analgesicduration.la <- read.xlsx("analgesicduration.la.xlsx")
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
m.complete.md

m.complete.sen.md <- metacont(n.e = ne,
                              mean.e = meane,
                              sd.e = sde,
                              n.c = nc,
                              mean.c = meanc,
                              sd.c = sdc,
                              studlab = study,
                              data = ttcomplete.sen,
                              sm = "MD",
                              fixed = FALSE,
                              random = TRUE,
                              method.tau = "REML",
                              method.random.ci = "HK",
                              title = "Time to Block")

m.complete.byla.md <- metacont(n.e = ne,
                          mean.e = meane,
                          sd.e = sde,
                          n.c = nc,
                          mean.c = meanc,
                          sd.c = sdc,
                          studlab = study,
                          data = ttcomplete.byla,
                          sm = "MD",
                          fixed = FALSE,
                          random = TRUE,
                          method.tau = "REML",
                          method.random.ci = "HK",
                          title = "Time to Block by Local")
m.complete.sen.byla.md <- metacont(n.e = ne,
                               mean.e = meane,
                               sd.e = sde,
                               n.c = nc,
                               mean.c = meanc,
                               sd.c = sdc,
                               studlab = study,
                               data = ttcomplete.sen.byla,
                               sm = "MD",
                               fixed = FALSE,
                               random = TRUE,
                               method.tau = "REML",
                               method.random.ci = "HK",
                               title = "Time to Block by Local")
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
                         sm = "MD",
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
m.sensoryduration.la.md <- metacont(n.e = ne,
                                 mean.e = meane,
                                 sd.e = sde,
                                 n.c = nc,
                                 mean.c = meanc,
                                 sd.c = sdc,
                                 studlab = study,
                                 data = sensoryduration.la,
                                 sm = "MD",
                                 fixed = FALSE,
                                 random = TRUE,
                                 method.tau = "REML",
                                 method.random.ci = "HK",
                                 title = "Duration of Motor Block")

m.sensoryduration.epi.md <- metacont(n.e = ne,
                                    mean.e = meane,
                                    sd.e = sde,
                                    n.c = nc,
                                    mean.c = meanc,
                                    sd.c = sdc,
                                    studlab = study,
                                    data = sensoryduration.la,
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
m.analgesicduration.la.md <- metacont(n.e = ne,
                                   mean.e = meane,
                                   sd.e = sde,
                                   n.c = nc,
                                   mean.c = meanc,
                                   sd.c = sdc,
                                   studlab = study,
                                   data = analgesicduration.la,
                                   sm = "MD",
                                   method.smd = "Hedges",
                                   fixed = FALSE,
                                   random = TRUE,
                                   method.tau = "REML",
                                   method.random.ci = "HK",
                                   title = "Duration of Analgesia")

m.failure.or <- metabin(event.e = ne, n.e = totale, event.c = nc, n.c = totalc, studlab = study, data = failure, sm = "OR", method = "MH", MH.exact = TRUE, random = TRUE, method.tau = "PM", method.random.ci = "HK", title = "Block Failure")



m.complete.ultrasound <- update(m.complete.md, subgroup = ultrasound, tau.common = TRUE)
m.complete.ultrasound
m.complete.ultrasound$pval.random.w #to see p value
m.complete.rob <- update(m.complete.md, subgroup = bias, tau.common = TRUE)
m.complete.rob
m.complete.rob$pval.random.w
m.complete.block <- update(m.complete.md, subgroup = blockgroup, tau.common = TRUE)
m.complete.block
m.complete.block$pval.random.w
m.complete.country <- update(m.complete.md, subgroup = devcountry, tau.common = TRUE)
m.complete.country
m.complete.country$pval.random.w
m.complete.byla.long <- update(m.complete.byla.md, subgroup = la, tau.common = TRUE)
m.complete.byla.long
m.complete.byla.long$pval.random.w
m.complete.byla.short <- update(m.complete.byla.md, subgroup = sa, tau.common = TRUE)
m.complete.byla.short
m.complete.byla.short$pval.random.w
m.complete.epi <- update(m.complete.md, subgroup = epi, tau.common = TRUE)
m.complete.epi
m.complete.epi$pval.random.w
m.sensoryduration.la.md <- update(m.sensoryduration.la.md, subgroup = samedose, tau.common = TRUE)
m.sensoryduration.la.md
m.sensoryduration.la.md$pval.random.w
m.sensoryduration.la.md$n.c.w
m.sensoryduration.la.md$n.e.w
m.sensoryduration.la.md$k.w
m.sensoryduration.la.md$I2.w
m.sensoryduration.la.md$lower.I2.w
m.sensoryduration.la.md$upper.I2.w
m.sensoryduration.epi.md <- update (m.sensoryduration.md, subgroup = epinephrine, tau.common = TRUE)
m.sensoryduration.epi.md
m.sensoryduration.epi.md$pval.random.w
m.sensoryduration.epi.md$n.c.w
m.sensoryduration.epi.md$n.e.w
m.sensoryduration.epi.md$k.w
m.sensoryduration.epi.md$I2.w
m.sensoryduration.epi.md$lower.I2.w
m.sensoryduration.epi.md$upper.I2.w
m.analgesicduration.la.md <- update(m.analgesicduration.la.md, subgroup = samedose, tau.common = TRUE)
m.analgesicduration.la.md
m.analgesicduration.la.md$pval.random.w
m.analgesicduration.la.md$n.c.w
m.analgesicduration.la.md$n.e.w
m.analgesicduration.la.md$k.w
m.analgesicduration.la.md$I2.w
m.analgesicduration.la.md$lower.I2.w
m.analgesicduration.la.md$upper.I2.w
m.analgesicduration.epi.md <- update(m.analgesicduration.md, subgroup = epinephrine, tau.common = TRUE)
m.analgesicduration.epi.md
m.analgesicduration.epi.md$pval.random.w
m.analgesicduration.epi.md$n.c.w
m.analgesicduration.epi.md$n.e.w
m.analgesicduration.epi.md$k.w
m.analgesicduration.epi.md$I2.w
m.analgesicduration.epi.md$lower.I2.w
m.analgesicduration.epi.md$upper.I2.w
m.motorduration.epi.md <- update(m.motorduration.md, subgroup = epinephrine, tau.common = TRUE)
m.motorduration.epi.md
m.motorduration.epi.md$pval.random.w
m.motorduration.epi.md$n.c.w
m.motorduration.epi.md$n.e.w
m.motorduration.epi.md$k.w
m.motorduration.epi.md$I2.w
m.motorduration.epi.md$lower.I2.w
m.motorduration.epi.md$upper.I2.w
m.complete.md.rma <- rma(yi = m.complete.md$TE, sei = m.complete.md$seTE, method = m.complete.md$method.tau, test = "knha")
m.complete.md.gosh <- gosh (m.complete.md.rma)
m.complete.diag <- gosh.diagnostics(m.complete.md.gosh)
m.complete.inf <- InfluenceAnalysis(m.complete.md, random = TRUE)
plot(m.complete.inf, "baujat")
plot(m.complete.inf, "influence")
plot(m.complete.inf, "es")
plot(m.complete.inf, "i2")


m.complete.sen.ultrasound <- update(m.complete.sen.md, subgroup = ultrasound, tau.common = TRUE)
m.complete.sen.ultrasound
m.complete.sen.ultrasound$pval.random.w
m.complete.sen.ultrasound$n.c.w
m.complete.sen.ultrasound$n.e.w
m.complete.sen.ultrasound$k.w
m.complete.sen.ultrasound$I2.w
m.complete.sen.ultrasound$lower.I2.w
m.complete.sen.ultrasound$upper.I2.w

m.complete.sen.rob <- update(m.complete.sen.md, subgroup = bias, tau.common = TRUE)
m.complete.sen.rob
m.complete.sen.rob$pval.random.w
m.complete.sen.rob$n.c.w
m.complete.sen.rob$n.e.w
m.complete.sen.rob$k.w
m.complete.sen.rob$I2.w
m.complete.sen.rob$lower.I2.w
m.complete.sen.rob$upper.I2.w

m.complete.sen.block <- update(m.complete.sen.md, subgroup = blockgroup, tau.common = TRUE)
m.complete.sen.block
m.complete.sen.block$pval.random.w
m.complete.sen.block$n.c.w
m.complete.sen.block$n.e.w
m.complete.sen.block$k.w
m.complete.sen.block$I2.w
m.complete.sen.block$lower.I2.w
m.complete.sen.block$upper.I2.w

m.complete.sen.country <- update(m.complete.sen.md, subgroup = devcountry, tau.common = TRUE)
m.complete.sen.country
m.complete.sen.country$pval.random.w
m.complete.sen.country$n.c.w
m.complete.sen.country$n.e.w
m.complete.sen.country$k.w
m.complete.sen.country$I2.w
m.complete.sen.country$lower.I2.w
m.complete.sen.country$upper.I2.w

m.complete.sen.byla.long <- update(m.complete.sen.byla.md, subgroup = la, tau.common = TRUE)
m.complete.sen.byla.long
m.complete.sen.byla.long$pval.random.w
m.complete.sen.byla.long$n.c.w
m.complete.sen.byla.long$n.e.w
m.complete.sen.byla.long$k.w
m.complete.sen.byla.long$I2.w
m.complete.sen.byla.long$lower.I2.w
m.complete.sen.byla.long$upper.I2.w

m.complete.sen.byla.short <- update(m.complete.sen.byla.md, subgroup = sa, tau.common = TRUE)
m.complete.sen.byla.short
m.complete.sen.byla.short$pval.random.w
m.complete.sen.byla.short$n.c.w
m.complete.sen.byla.short$n.e.w
m.complete.sen.byla.short$k.w
m.complete.sen.byla.short$I2.w
m.complete.sen.byla.short$lower.I2.w
m.complete.sen.byla.short$upper.I2.w

m.complete.sen.epi <- update(m.complete.sen.md, subgroup = epi, tau.common = TRUE)
m.complete.sen.epi
m.complete.sen.epi$pval.random.w
m.complete.sen.epi$n.c.w
m.complete.sen.epi$n.e.w
m.complete.sen.epi$k.w
m.complete.sen.epi$I2.w
m.complete.sen.epi$lower.I2.w
m.complete.sen.epi$upper.I2.w

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
             digits.sd = 1,)

meta::forest(m.complete.ultrasound, 
             subgroup.name = "Ultrasound used",             
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long acting",
             label.left = "Mixture faster",
             label.right = "Long-acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1,)
meta::forest(m.complete.block,
             sortvar = studlab,
             subgroup.name = "Block location",
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture faster",
             label.right = "Long acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1,)
meta::forest(m.complete.epi,
             subgroup.name = "Epinephrine used",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture faster",
             label.right = "Long acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1,)
meta::forest(m.complete.rob,
             subgroup.name = "Risk of bias",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture faster",
             label.right = "Long-acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)
meta::forest(m.complete.country,
             subgroup.name = "Country",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture faster",
             label.right = "Long-acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)
meta::forest(m.complete.byla.long,
             subgroup.name = "Long-acting local anesthetic",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed-local",
             label.c = "Long-acting",
             label.left = "Mixture Faster",
             label.right = "Long-acting Faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)
meta::forest(m.complete.byla.short,
             subgroup.name = "Short-acting local anesthetic",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture faster",
             label.right = "Long-acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)


###############################

meta::forest(m.complete.sen.md, 
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
             digits.sd = 1,)

meta::forest(m.complete.sen.ultrasound, 
             subgroup.name = "Ultrasound used",             
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long acting",
             label.left = "Mixture faster",
             label.right = "Long-acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1,)
meta::forest(m.complete.sen.block,
             sortvar = studlab,
             subgroup.name = "Block location",
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture faster",
             label.right = "Long acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1,)
meta::forest(m.complete.sen.epi,
             subgroup.name = "Epinephrine used",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture faster",
             label.right = "Long acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1,)
meta::forest(m.complete.sen.rob,
             subgroup.name = "Risk of bias",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture faster",
             label.right = "Long-acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1,
             sort.subgroup = FALSE)
meta::forest(m.complete.sen.country,
             subgroup.name = "Country",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture faster",
             label.right = "Long-acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)
meta::forest(m.complete.sen.byla.long,
             subgroup.name = "Long-acting local anesthetic",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed-local",
             label.c = "Long-acting",
             label.left = "Mixture Faster",
             label.right = "Long-acting Faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)
meta::forest(m.complete.sen.byla.short,
             subgroup.name = "Short-acting local anesthetic",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture faster",
             label.right = "Long-acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)

###################

meta::forest(m.ttsensory.md, 
             prediction = TRUE, 
             print.tau2 = FALSE,
             sortvar = studlab,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture faster",
             label.right = "Long-acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)

meta::forest(m.sensoryduration.md,
             prediction = TRUE, 
             print.tau2 = FALSE,
             sortvar = studlab,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Long-acting longer",
             label.right = "Mixed local longer",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)
meta::forest(m.sensoryduration.epi.md,
             subgroup.name = "Epinephrine used",
             prediction = TRUE, 
             print.tau2 = FALSE,
             sortvar = studlab,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Long-acting longer",
             label.right = "Mixed local longer",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)
meta::forest(m.sensoryduration.la.md,
             subgroup.name = "Long-acting local dose in mixture vs long-acting only",
             prediction = TRUE, 
             print.tau2 = FALSE,
             sortvar = studlab,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Long-acting longer",
             label.right = "Mixed local longer",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)

meta::forest(m.ttmotor.md, 
             prediction = TRUE, 
             print.tau2 = FALSE,
             sortvar = studlab,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixed local faster",
             label.right = "Long-acting faster",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)

meta::forest(m.motorduration.md, 
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Long-acting longer",
             label.right = "Mixed Local longer",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)

meta::forest(m.motorduration.epi.md,
             subgroup.name = "Epinephrine use",
             prediction = TRUE, 
             print.tau2 = FALSE,
             sortvar = studlab,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Long-acting longer",
             label.right = "Mixed local longer",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)

meta::forest(m.analgesicduration.md, 
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Long-acting longer",
             label.right = "Mixed Local longer",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1)

meta::forest(m.analgesicduration.epi.md, 
             subgroup.name = "Epinephrine used",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long-acting",
             label.left = "Mixture shorter",
             label.right = "Long acting shorter",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1,)

meta::forest(m.analgesicduration.la.md, 
             subgroup.name = "Long-acting local dose in mixture vs long-acting only",
             sortvar = studlab,
             prediction = TRUE, 
             print.tau2 = FALSE,
             label.e = "Mixed local",
             label.c = "Long acting",
             label.left = "Mixture shorter",
             label.right = "Long acting shorter",
             digits = 1,
             digits.se = 1,
             digits.mean = 1,
             digits.sd = 1,)

m.bias <- metabias(m.complete.md, method.bias = "Egger")
m.bias
find.outliers(m.complete.md)
m.complete.nooutlier <- update(m.complete.md, exclude = c(3, 4, 7, 9, 12, 13, 18))
m.complete.nooutlier
meta::funnel(m.complete.md, studlab = TRUE)
col.contour = c("gray75", "gray85", "gray95")
meta::funnel(m.complete.md,
             xlim = c(-40, 10),
             contour = c(0.9, 0.95, 0.99),
             col.contour = col.contour,
             studlab = TRUE
)
legend(x = -30, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)
title("Contour-Enhanced Funnel Plot")

tf <- trimfill(m.complete.md)
