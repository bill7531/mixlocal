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
ttcomplete.sen <- read.xlsx("ttcomplete-sen.xlsx") #this is the primary outcome
ttcomplete.sen.bypotency <- read.xlsx("ttcomplete.sen.bypotency.xlsx")
ttcomplete.sen.bypotency.ul <- read.xlsx("ttcomplete.sen.bypotency.ul.xlsx")
ttcomplete.sen.bypotency.ll <- read.xlsx("ttcomplete.sen.bypotency.ll.xlsx")


ttcomplete.byla <- read.xlsx("ttcomplete.byla.xlsx")
ttcomplete.sen.byla <- read.xlsx("ttcomplete.sen.byla.xlsx")
ttsensory <- read.xlsx("ttsensory.xlsx")
completeblock <- read.xlsx("ttcompleteonly.xlsx")
completesensory <- read.xlsx("completesensory.xlsx")
ttmotor <- read.xlsx("ttmotor.xlsx") #only 3 studies, no subgroup analysis
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
                          common = FALSE,
                          random = TRUE,
                          method.tau = "REML",
                          method.random.ci = "HK",
                          title = "Time to Block")
m.complete.md

#this is primary
m.complete.sen.md <- metacont(n.e = ne,
                              mean.e = meane,
                              sd.e = sde,
                              n.c = nc,
                              mean.c = meanc,
                              sd.c = sdc,
                              studlab = study,
                              data = ttcomplete.sen,
                              sm = "MD",
                              common = FALSE,
                              random = TRUE,
                              method.tau = "REML",
                              method.random.ci = "HK",
                              title = "Time to Block")
m.complete.sen.bypotency.md <- metacont(n.e = ne,
                              mean.e = meane,
                              sd.e = sde,
                              n.c = nc,
                              mean.c = meanc,
                              sd.c = sdc,
                              studlab = study,
                              data = ttcomplete.sen.bypotency,
                              sm = "MD",
                              common = FALSE,
                              random = TRUE,
                              method.tau = "REML",
                              method.random.ci = "HK",
                              title = "Time to Block by Potency")

m.complete.sen.bypotency.ul.md <- metacont(n.e = ne,
                                        mean.e = meane,
                                        sd.e = sde,
                                        n.c = nc,
                                        mean.c = meanc,
                                        sd.c = sdc,
                                        studlab = study,
                                        data = ttcomplete.sen.bypotency.ul,
                                        sm = "MD",
                                        common = FALSE,
                                        random = TRUE,
                                        method.tau = "REML",
                                        method.random.ci = "HK",
                                        title = "Time to Block by Potency Upper Limb")

m.complete.sen.bypotency.ll.md <- metacont(n.e = ne,
                                        mean.e = meane,
                                        sd.e = sde,
                                        n.c = nc,
                                        mean.c = meanc,
                                        sd.c = sdc,
                                        studlab = study,
                                        data = ttcomplete.sen.bypotency.ll,
                                        sm = "MD",
                                        common = FALSE,
                                        random = TRUE,
                                        method.tau = "REML",
                                        method.random.ci = "HK",
                                        title = "Time to Block by Potency Lower Limb")

m.complete.byla.md <- metacont(n.e = ne,
                          mean.e = meane,
                          sd.e = sde,
                          n.c = nc,
                          mean.c = meanc,
                          sd.c = sdc,
                          studlab = study,
                          data = ttcomplete.byla,
                          sm = "MD",
                          common = FALSE,
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
                               common = FALSE,
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
                           common = FALSE,
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
                                 common = FALSE,
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
                         common = FALSE,
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
                               common = FALSE,
                               random = TRUE,
                               method.tau = "REML",
                               method.random.ci = "HK",
                               title = "Time to Complete Motor Block")
m.completeblock.md <- metacont(n.e = ne,
                               mean.e = meane,
                               sd.e = sde,
                               n.c = nc,
                               mean.c = meanc,
                               sd.c = sdc,
                               studlab = study,
                               data = completeblock,
                               sm = "MD",
                               common = FALSE,
                               random = TRUE,
                               method.tau = "REML",
                               method.random.ci = "HK",
                               title = "Time to Complete Block")

m.sensoryduration.md <- metacont(n.e = ne,
                                 mean.e = meane,
                                 sd.e = sde,
                                 n.c = nc,
                                 mean.c = meanc,
                                 sd.c = sdc,
                                 studlab = study,
                                 data = sensoryduration,
                                 sm = "MD",
                                 common = FALSE,
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
                                 common = FALSE,
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
                                    common = FALSE,
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
                               common = FALSE,
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
                                   common = FALSE,
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
                                   common = FALSE,
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

#sensory block latency subgroups
m.ttsensory.ultrasound <- update(m.ttsensory.md, subgroup = ultrasound, tau.common = TRUE)
m.ttsensory.ultrasound
m.ttsensory.ultrasound$pval.random.w
m.ttsensory.ultrasound$n.c.w
m.ttsensory.ultrasound$n.e.w
m.ttsensory.ultrasound$I2.w
m.ttsensory.ultrasound$lower.I2.w
m.ttsensory.ultrasound$upper.I2.w

m.ttsensory.block <- update(m.ttsensory.md, subgroup = blockgroup, tau.common = TRUE)
m.ttsensory.block
m.ttsensory.block$pval.random.w
m.ttsensory.block$n.c.w
m.ttsensory.block$n.e.w
m.ttsensory.block$I2.w
m.ttsensory.block$lower.I2.w
m.ttsensory.block$upper.I2.w

m.ttsensory.epi <- update(m.ttsensory.md, subgroup = epi, tau.common = TRUE)
m.ttsensory.epi
m.ttsensory.epi$pval.random.w
m.ttsensory.epi$n.c.w
m.ttsensory.epi$n.e.w
m.ttsensory.epi$I2.w
m.ttsensory.epi$lower.I2.w
m.ttsensory.epi$upper.I2.w

m.ttsensory.la <- update(m.ttsensory.md, subgroup = la, tau.common = TRUE)
m.ttsensory.la
m.ttsensory.la$pval.random.w
m.ttsensory.la$n.c.w
m.ttsensory.la$n.e.w
m.ttsensory.la$I2.w
m.ttsensory.la$lower.I2.w
m.ttsensory.la$upper.I2.w

m.ttsensory.sa <- update(m.ttsensory.md, subgroup = sa, tau.common = TRUE)
m.ttsensory.sa
m.ttsensory.sa$pval.random.w
m.ttsensory.sa$n.c.w
m.ttsensory.sa$n.e.w
m.ttsensory.sa$I2.w
m.ttsensory.sa$lower.I2.w
m.ttsensory.sa$upper.I2.w

m.ttsensory.bias <- update(m.ttsensory.md, subgroup = bias, tau.common = TRUE)
m.ttsensory.bias
m.ttsensory.bias$pval.random.w
m.ttsensory.bias$n.c.w
m.ttsensory.bias$n.e.w
m.ttsensory.bias$I2.w
m.ttsensory.bias$lower.I2.w
m.ttsensory.bias$upper.I2.w

m.ttsensory.dev <- update(m.ttsensory.md, subgroup = devcountry, tau.common = TRUE)
m.ttsensory.dev
m.ttsensory.dev$pval.random.w
m.ttsensory.dev$n.c.w
m.ttsensory.dev$n.e.w
m.ttsensory.dev$I2.w
m.ttsensory.dev$lower.I2.w
m.ttsensory.dev$upper.I2.w

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
m.analgesicduration.epi.md <- update(m.analgesicduration.md, subgroup = epi, tau.common = TRUE)
m.analgesicduration.epi.md
m.analgesicduration.epi.md$pval.random.w
m.analgesicduration.epi.md$n.c.w
m.analgesicduration.epi.md$n.e.w
m.analgesicduration.epi.md$k.w
m.analgesicduration.epi.md$I2.w
m.analgesicduration.epi.md$lower.I2.w
m.analgesicduration.epi.md$upper.I2.w
m.motorduration.epi.md <- update(m.motorduration.md, subgroup = epi, tau.common = TRUE)
m.motorduration.epi.md
m.motorduration.epi.md$pval.random.w
m.motorduration.epi.md$n.c.w
m.motorduration.epi.md$n.e.w
m.motorduration.epi.md$k.w
m.motorduration.epi.md$I2.w
m.motorduration.epi.md$lower.I2.w
m.motorduration.epi.md$upper.I2.w
#m.complete.sen.md.rma <- rma(yi = m.complete.sen.md$TE, sei = m.complete.sen.md$seTE, method = m.complete.sen.md$method.tau, test = "knha")
#m.complete.sen.md.gosh <- gosh (m.complete.md.rma)
#m.complete.diag <- gosh.diagnostics(m.complete.md.gosh)



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

#compele block subgroups
m.completeblock.ultrasound <- update(m.completeblock.md, subgroup = ultrasound, tau.common = TRUE)
m.completeblock.ultrasound
m.completeblock.ultrasound$pval.random.w
m.completeblock.ultrasound$n.c.w
m.completeblock.ultrasound$n.e.w
m.completeblock.ultrasound$I2.w
m.completeblock.ultrasound$lower.I2.w
m.completeblock.ultrasound$upper.I2.w

m.completeblock.block <- update(m.completeblock.md, subgroup = blockgroup, tau.common = TRUE)
m.completeblock.block
m.completeblock.block$pval.random.w
m.completeblock.block$n.c.w
m.completeblock.block$n.e.w
m.completeblock.block$I2.w
m.completeblock.block$lower.I2.w
m.completeblock.block$upper.I2.w

m.completeblock.epi <- update(m.completeblock.md, subgroup = epi, tau.common = TRUE)
m.completeblock.epi
m.completeblock.epi$pval.random.w
m.completeblock.epi$n.c.w
m.completeblock.epi$n.e.w
m.completeblock.epi$I2.w
m.completeblock.epi$lower.I2.w
m.completeblock.epi$upper.I2.w

m.completeblock.la <- update(m.completeblock.md, subgroup = la, tau.common = TRUE)
m.completeblock.la
m.completeblock.la$pval.random.w
m.completeblock.la$n.c.w
m.completeblock.la$n.e.w
m.completeblock.la$I2.w
m.completeblock.la$lower.I2.w
m.completeblock.la$upper.I2.w

m.completeblock.sa <- update(m.completeblock.md, subgroup = sa, tau.common = TRUE)
m.completeblock.sa
m.completeblock.sa$pval.random.w
m.completeblock.sa$n.c.w
m.completeblock.sa$n.e.w
m.completeblock.sa$I2.w
m.completeblock.sa$lower.I2.w
m.completeblock.sa$upper.I2.w

m.completeblock.bias <- update(m.completeblock.md, subgroup = bias, tau.common = TRUE)
m.completeblock.bias
m.completeblock.bias$pval.random.w
m.completeblock.bias$n.c.w
m.completeblock.bias$n.e.w
m.completeblock.bias$I2.w
m.completeblock.bias$lower.I2.w
m.completeblock.bias$upper.I2.w

m.completeblock.dev <- update(m.completeblock.md, subgroup = devcountry, tau.common = TRUE)
m.completeblock.dev
m.completeblock.dev$pval.random.w
m.completeblock.dev$n.c.w
m.completeblock.dev$n.e.w
m.completeblock.dev$I2.w
m.completeblock.dev$lower.I2.w
m.completeblock.dev$upper.I2.w


#sensory block latency subgroups
m.ttsensory.ultrasound <- update(m.ttsensory.md, subgroup = ultrasound, tau.common = TRUE)
m.ttsensory.ultrasound
m.ttsensory.ultrasound$pval.random.w
m.ttsensory.ultrasound$n.c.w
m.ttsensory.ultrasound$n.e.w
m.ttsensory.ultrasound$I2.w
m.ttsensory.ultrasound$lower.I2.w
m.ttsensory.ultrasound$upper.I2.w

m.ttsensory.block <- update(m.ttsensory.md, subgroup = blockgroup, tau.common = TRUE)
m.ttsensory.block
m.ttsensory.block$pval.random.w
m.ttsensory.block$n.c.w
m.ttsensory.block$n.e.w
m.ttsensory.block$I2.w
m.ttsensory.block$lower.I2.w
m.ttsensory.block$upper.I2.w

m.ttsensory.epi <- update(m.ttsensory.md, subgroup = epi, tau.common = TRUE)
m.ttsensory.epi
m.ttsensory.epi$pval.random.w
m.ttsensory.epi$n.c.w
m.ttsensory.epi$n.e.w
m.ttsensory.epi$I2.w
m.ttsensory.epi$lower.I2.w
m.ttsensory.epi$upper.I2.w

m.ttsensory.la <- update(m.ttsensory.md, subgroup = la, tau.common = TRUE)
m.ttsensory.la
m.ttsensory.la$pval.random.w
m.ttsensory.la$n.c.w
m.ttsensory.la$n.e.w
m.ttsensory.la$I2.w
m.ttsensory.la$lower.I2.w
m.ttsensory.la$upper.I2.w

m.ttsensory.sa <- update(m.ttsensory.md, subgroup = sa, tau.common = TRUE)
m.ttsensory.sa
m.ttsensory.sa$pval.random.w
m.ttsensory.sa$n.c.w
m.ttsensory.sa$n.e.w
m.ttsensory.sa$I2.w
m.ttsensory.sa$lower.I2.w
m.ttsensory.sa$upper.I2.w

m.ttsensory.bias <- update(m.ttsensory.md, subgroup = bias, tau.common = TRUE)
m.ttsensory.bias
m.ttsensory.bias$pval.random.w
m.ttsensory.bias$n.c.w
m.ttsensory.bias$n.e.w
m.ttsensory.bias$I2.w
m.ttsensory.bias$lower.I2.w
m.ttsensory.bias$upper.I2.w

m.ttsensory.dev <- update(m.ttsensory.md, subgroup = devcountry, tau.common = TRUE)
m.ttsensory.dev
m.ttsensory.dev$pval.random.w
m.ttsensory.dev$n.c.w
m.ttsensory.dev$n.e.w
m.ttsensory.dev$I2.w
m.ttsensory.dev$lower.I2.w
m.ttsensory.dev$upper.I2.w


#complete sensory block subgroups
m.completesensory.ultrasound <- update(m.completesensory.md, subgroup = ultrasound, tau.common = TRUE)
m.completesensory.ultrasound
m.completesensory.ultrasound$pval.random.w
m.completesensory.ultrasound$n.c.w
m.completesensory.ultrasound$n.e.w
m.completesensory.ultrasound$I2.w
m.completesensory.ultrasound$lower.I2.w
m.completesensory.ultrasound$upper.I2.w

m.completesensory.block <- update(m.completesensory.md, subgroup = blockgroup, tau.common = TRUE)
m.completesensory.block
m.completesensory.block$pval.random.w
m.completesensory.block$n.c.w
m.completesensory.block$n.e.w
m.completesensory.block$I2.w
m.completesensory.block$lower.I2.w
m.completesensory.block$upper.I2.w

m.completesensory.epi <- update(m.completesensory.md, subgroup = epi, tau.common = TRUE)
m.completesensory.epi
m.completesensory.epi$pval.random.w
m.completesensory.epi$n.c.w
m.completesensory.epi$n.e.w
m.completesensory.epi$I2.w
m.completesensory.epi$lower.I2.w
m.completesensory.epi$upper.I2.w

m.completesensory.la <- update(m.completesensory.md, subgroup = la, tau.common = TRUE)
m.completesensory.la
m.completesensory.la$pval.random.w
m.completesensory.la$n.c.w
m.completesensory.la$n.e.w
m.completesensory.la$I2.w
m.completesensory.la$lower.I2.w
m.completesensory.la$upper.I2.w

m.completesensory.sa <- update(m.completesensory.md, subgroup = sa, tau.common = TRUE)
m.completesensory.sa
m.completesensory.sa$pval.random.w
m.completesensory.sa$n.c.w
m.completesensory.sa$n.e.w
m.completesensory.sa$I2.w
m.completesensory.sa$lower.I2.w
m.completesensory.sa$upper.I2.w

m.completesensory.bias <- update(m.completesensory.md, subgroup = bias, tau.common = TRUE)
m.completesensory.bias
m.completesensory.bias$pval.random.w
m.completesensory.bias$n.c.w
m.completesensory.bias$n.e.w
m.completesensory.bias$I2.w
m.completesensory.bias$lower.I2.w
m.completesensory.bias$upper.I2.w

m.completesensory.dev <- update(m.completesensory.md, subgroup = devcountry, tau.common = TRUE)
m.completesensory.dev
m.completesensory.dev$pval.random.w
m.completesensory.dev$n.c.w
m.completesensory.dev$n.e.w
m.completesensory.dev$I2.w
m.completesensory.dev$lower.I2.w
m.completesensory.dev$upper.I2.w

#complete motor block subgroups
m.completemotor.ultrasound <- update(m.completemotor.md, subgroup = ultrasound, tau.common = TRUE)
m.completemotor.ultrasound
m.completemotor.ultrasound$pval.random.w
m.completemotor.ultrasound$n.c.w
m.completemotor.ultrasound$n.e.w
m.completemotor.ultrasound$I2.w
m.completemotor.ultrasound$lower.I2.w
m.completemotor.ultrasound$upper.I2.w

m.completemotor.block <- update(m.completemotor.md, subgroup = blockgroup, tau.common = TRUE)
m.completemotor.block
m.completemotor.block$pval.random.w
m.completemotor.block$n.c.w
m.completemotor.block$n.e.w
m.completemotor.block$I2.w
m.completemotor.block$lower.I2.w
m.completemotor.block$upper.I2.w

m.completemotor.epi <- update(m.completemotor.md, subgroup = epi, tau.common = TRUE)
m.completemotor.epi
m.completemotor.epi$pval.random.w
m.completemotor.epi$n.c.w
m.completemotor.epi$n.e.w
m.completemotor.epi$I2.w
m.completemotor.epi$lower.I2.w
m.completemotor.epi$upper.I2.w

m.completemotor.la <- update(m.completemotor.md, subgroup = la, tau.common = TRUE)
m.completemotor.la
m.completemotor.la$pval.random.w
m.completemotor.la$n.c.w
m.completemotor.la$n.e.w
m.completemotor.la$I2.w
m.completemotor.la$lower.I2.w
m.completemotor.la$upper.I2.w

m.completemotor.sa <- update(m.completemotor.md, subgroup = sa, tau.common = TRUE)
m.completemotor.sa
m.completemotor.sa$pval.random.w
m.completemotor.sa$n.c.w
m.completemotor.sa$n.e.w
m.completemotor.sa$I2.w
m.completemotor.sa$lower.I2.w
m.completemotor.sa$upper.I2.w

m.completemotor.bias <- update(m.completemotor.md, subgroup = bias, tau.common = TRUE)
m.completemotor.bias
m.completemotor.bias$pval.random.w
m.completemotor.bias$n.c.w
m.completemotor.bias$n.e.w
m.completemotor.bias$I2.w
m.completemotor.bias$lower.I2.w
m.completemotor.bias$upper.I2.w

m.completemotor.dev <- update(m.completemotor.md, subgroup = devcountry, tau.common = TRUE)
m.completemotor.dev
m.completemotor.dev$pval.random.w
m.completemotor.dev$n.c.w
m.completemotor.dev$n.e.w
m.completemotor.dev$I2.w
m.completemotor.dev$lower.I2.w
m.completemotor.dev$upper.I2.w

#sensory duration subgroups
m.sensoryduration.ultrasound <- update(m.sensoryduration.md, subgroup = ultrasound, tau.common = TRUE)
m.sensoryduration.ultrasound
m.sensoryduration.ultrasound$pval.random.w
m.sensoryduration.ultrasound$n.c.w
m.sensoryduration.ultrasound$n.e.w
m.sensoryduration.ultrasound$I2.w
m.sensoryduration.ultrasound$lower.I2.w
m.sensoryduration.ultrasound$upper.I2.w

m.sensoryduration.block <- update(m.sensoryduration.md, subgroup = blockgroup, tau.common = TRUE)
m.sensoryduration.block
m.sensoryduration.block$pval.random.w
m.sensoryduration.block$n.c.w
m.sensoryduration.block$n.e.w
m.sensoryduration.block$I2.w
m.sensoryduration.block$lower.I2.w
m.sensoryduration.block$upper.I2.w

m.sensoryduration.epi <- update(m.sensoryduration.md, subgroup = epi, tau.common = TRUE)
m.sensoryduration.epi
m.sensoryduration.epi$pval.random.w
m.sensoryduration.epi$n.c.w
m.sensoryduration.epi$n.e.w
m.sensoryduration.epi$I2.w
m.sensoryduration.epi$lower.I2.w
m.sensoryduration.epi$upper.I2.w

m.sensoryduration.la <- update(m.sensoryduration.md, subgroup = la, tau.common = TRUE)
m.sensoryduration.la
m.sensoryduration.la$pval.random.w
m.sensoryduration.la$n.c.w
m.sensoryduration.la$n.e.w
m.sensoryduration.la$I2.w
m.sensoryduration.la$lower.I2.w
m.sensoryduration.la$upper.I2.w

m.sensoryduration.sa <- update(m.sensoryduration.md, subgroup = sa, tau.common = TRUE)
m.sensoryduration.sa
m.sensoryduration.sa$pval.random.w
m.sensoryduration.sa$n.c.w
m.sensoryduration.sa$n.e.w
m.sensoryduration.sa$I2.w
m.sensoryduration.sa$lower.I2.w
m.sensoryduration.sa$upper.I2.w

m.sensoryduration.bias <- update(m.sensoryduration.md, subgroup = bias, tau.common = TRUE)
m.sensoryduration.bias
m.sensoryduration.bias$pval.random.w
m.sensoryduration.bias$n.c.w
m.sensoryduration.bias$n.e.w
m.sensoryduration.bias$I2.w
m.sensoryduration.bias$lower.I2.w
m.sensoryduration.bias$upper.I2.w

m.sensoryduration.dev <- update(m.sensoryduration.md, subgroup = devcountry, tau.common = TRUE)
m.sensoryduration.dev
m.sensoryduration.dev$pval.random.w
m.sensoryduration.dev$n.c.w
m.sensoryduration.dev$n.e.w
m.sensoryduration.dev$I2.w
m.sensoryduration.dev$lower.I2.w
m.sensoryduration.dev$upper.I2.w

#motor duration subgroups
m.motorduration.ultrasound <- update(m.motorduration.md, subgroup = ultrasound, tau.common = TRUE)
m.motorduration.ultrasound
m.motorduration.ultrasound$pval.random.w
m.motorduration.ultrasound$n.c.w
m.motorduration.ultrasound$n.e.w
m.motorduration.ultrasound$I2.w
m.motorduration.ultrasound$lower.I2.w
m.motorduration.ultrasound$upper.I2.w

m.motorduration.block <- update(m.motorduration.md, subgroup = Block.Group, tau.common = TRUE)
m.motorduration.block
m.motorduration.block$pval.random.w
m.motorduration.block$n.c.w
m.motorduration.block$n.e.w
m.motorduration.block$I2.w
m.motorduration.block$lower.I2.w
m.motorduration.block$upper.I2.w

m.motorduration.epi <- update(m.motorduration.md, subgroup = epi, tau.common = TRUE)
m.motorduration.epi
m.motorduration.epi$pval.random.w
m.motorduration.epi$n.c.w
m.motorduration.epi$n.e.w
m.motorduration.epi$I2.w
m.motorduration.epi$lower.I2.w
m.motorduration.epi$upper.I2.w

m.motorduration.la <- update(m.motorduration.md, subgroup = la, tau.common = TRUE)
m.motorduration.la
m.motorduration.la$pval.random.w
m.motorduration.la$n.c.w
m.motorduration.la$n.e.w
m.motorduration.la$I2.w
m.motorduration.la$lower.I2.w
m.motorduration.la$upper.I2.w

m.motorduration.sa <- update(m.motorduration.md, subgroup = sa, tau.common = TRUE)
m.motorduration.sa
m.motorduration.sa$pval.random.w
m.motorduration.sa$n.c.w
m.motorduration.sa$n.e.w
m.motorduration.sa$I2.w
m.motorduration.sa$lower.I2.w
m.motorduration.sa$upper.I2.w

m.motorduration.bias <- update(m.motorduration.md, subgroup = bias, tau.common = TRUE)
m.motorduration.bias
m.motorduration.bias$pval.random.w
m.motorduration.bias$n.c.w
m.motorduration.bias$n.e.w
m.motorduration.bias$I2.w
m.motorduration.bias$lower.I2.w
m.motorduration.bias$upper.I2.w

m.motorduration.dev <- update(m.motorduration.md, subgroup = devcountry, tau.common = TRUE)
m.motorduration.dev
m.motorduration.dev$pval.random.w
m.motorduration.dev$n.c.w
m.motorduration.dev$n.e.w
m.motorduration.dev$I2.w
m.motorduration.dev$lower.I2.w
m.motorduration.dev$upper.I2.w

#analgesic duration subgroups
m.analgesicduration.ultrasound <- update(m.analgesicduration.md, subgroup = ultrasound, tau.common = TRUE)
m.analgesicduration.ultrasound
m.analgesicduration.ultrasound$pval.random.w
m.analgesicduration.ultrasound$n.c.w
m.analgesicduration.ultrasound$n.e.w
m.analgesicduration.ultrasound$I2.w
m.analgesicduration.ultrasound$lower.I2.w
m.analgesicduration.ultrasound$upper.I2.w

m.analgesicduration.block <- update(m.analgesicduration.md, subgroup = blockgroup, tau.common = TRUE)
m.analgesicduration.block
m.analgesicduration.block$pval.random.w
m.analgesicduration.block$n.c.w
m.analgesicduration.block$n.e.w
m.analgesicduration.block$I2.w
m.analgesicduration.block$lower.I2.w
m.analgesicduration.block$upper.I2.w

m.analgesicduration.epi <- update(m.analgesicduration.md, subgroup = epi, tau.common = TRUE)
m.analgesicduration.epi
m.analgesicduration.epi$pval.random.w
m.analgesicduration.epi$n.c.w
m.analgesicduration.epi$n.e.w
m.analgesicduration.epi$I2.w
m.analgesicduration.epi$lower.I2.w
m.analgesicduration.epi$upper.I2.w

m.analgesicduration.la <- update(m.analgesicduration.md, subgroup = la, tau.common = TRUE)
m.analgesicduration.la
m.analgesicduration.la$pval.random.w
m.analgesicduration.la$n.c.w
m.analgesicduration.la$n.e.w
m.analgesicduration.la$I2.w
m.analgesicduration.la$lower.I2.w
m.analgesicduration.la$upper.I2.w

m.analgesicduration.sa <- update(m.analgesicduration.md, subgroup = sa, tau.common = TRUE)
m.analgesicduration.sa
m.analgesicduration.sa$pval.random.w
m.analgesicduration.sa$n.c.w
m.analgesicduration.sa$n.e.w
m.analgesicduration.sa$I2.w
m.analgesicduration.sa$lower.I2.w
m.analgesicduration.sa$upper.I2.w

m.analgesicduration.bias <- update(m.analgesicduration.md, subgroup = bias, tau.common = TRUE)
m.analgesicduration.bias
m.analgesicduration.bias$pval.random.w
m.analgesicduration.bias$n.c.w
m.analgesicduration.bias$n.e.w
m.analgesicduration.bias$I2.w
m.analgesicduration.bias$lower.I2.w
m.analgesicduration.bias$upper.I2.w

m.analgesicduration.dev <- update(m.analgesicduration.md, subgroup = devcountry, tau.common = TRUE)
m.analgesicduration.dev
m.analgesicduration.dev$pval.random.w
m.analgesicduration.dev$n.c.w
m.analgesicduration.dev$n.e.w
m.analgesicduration.dev$I2.w
m.analgesicduration.dev$lower.I2.w
m.analgesicduration.dev$upper.I2.w

m.analgesicduration.time <- update(m.analgesicduration.md, subgroup = timepoint, tau.common = TRUE)
m.analgesicduration.time
m.analgesicduration.time$pval.random.w
m.analgesicduration.time$n.c.w
m.analgesicduration.time$n.e.w
m.analgesicduration.time$I2.w
m.analgesicduration.time$lower.I2.w
m.analgesicduration.time$upper.I2.w


#m.complete.sen.bypotency <- update(m.complete.sen.bypotency.md, subgroup = potencyratio, tau.common = TRUE)
m.complete.sen.bypotency.reg <- metareg(m.complete.sen.bypotency.md, ~logpotency)
m.complete.sen.bypotency.ul.reg <- metareg(m.complete.sen.bypotency.ul.md, ~logpotency)
m.complete.sen.bypotency.ll.reg <- metareg(m.complete.sen.bypotency.ll.md, ~logpotency)
bubble(m.complete.sen.bypotency.reg, studlab = TRUE)
bubble(m.complete.sen.bypotency.ul.reg, studlab = TRUE)
bubble(m.complete.sen.bypotency.ll.reg, studlab = TRUE)
m.complete.sen.bypotency.reg
m.complete.sen.bypotency.ul.reg
m.complete.sen.bypotency.ll.reg

m.complete.year.reg <- metareg(m.complete.md, ~year)
m.complete.year.reg

m.ttsensory.reg <- metareg(m.ttsensory.md, ~year)
m.ttsensory.reg

m.ttmotor.reg <- metareg(m.ttmotor.md, ~year)
m.ttmotor.reg

m.completesnsory.reg <- metareg(m.completesensory.md, ~year)
m.completesnsory.reg

m.completemotor.reg <- metareg(m.completemotor.md, ~year)
m.completemotor.reg

m.sensoryduration.reg <- metareg(m.sensoryduration.md, ~year)
m.sensoryduration.reg

m.motorduration.reg <- metareg(m.motorduration.md, ~year)
m.motorduration.reg

m.analgesicduration.reg <- metareg(m.analgesicduration.md, ~year)
m.analgesicduration.reg

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
             digits.sd = 1,
             layout = "RevMan5")
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

m.bias <- metabias(m.complete.sen.md, method.bias = "Egger")
m.bias
find.outliers(m.complete.sen.md)
m.complete.sen.nooutlier <- update(m.complete.sen.md, exclude = c(3, 4, 7, 9, 12, 13, 18))
m.complete.sen.nooutlier
meta::funnel(m.complete.sen.md, studlab = FALSE)
col.contour = c("gray75", "gray85", "gray95")
meta::funnel(m.complete.sen.md,
             xlim = c(-30, 10),
             contour = c(0.9, 0.95, 0.99),
             col.contour = col.contour,
             
)
legend(x = -30, y = 0, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)
title("Contour-Enhanced Funnel Plot")

tf <- trimfill(m.complete.md)

#sensitivity
find.outliers(m.complete.sen.md)
m.complete.sen.inf <- InfluenceAnalysis(m.complete.sen.md, random = TRUE)
plot(m.complete.sen.inf, "baujat")
plot(m.complete.sen.inf, "influence")
update(m.complete.sen.md, exclude = c(1, 5, 15, 18, 19)) %>% 
  summary()


m.complete.sen.rma <- rma(yi = m.complete.sen.md$TE,
                          sei = m.complete.sen.md$seTE,
                          method = m.complete.sen.md$method.tau,
                          test = "knha")

#m.complete.sen.gosh <- gosh(m.complete.sen.rma)
#plot(m.complete.sen.gosh)
#m.complete.sen.gosh.diag <- gosh.diagnostics(m.complete.sen.gosh, km = FALSE, db = FALSE, gmm = TRUE)
