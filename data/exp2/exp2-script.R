### load packages for data import, analysis, and visualisation

library(ggplot2)
library(RColorBrewer)
library(Rmisc)
library(lme4)
library(lmerTest)
library(data.table)
# library(extrafont)

### import data from github
###### the variable "SCALE" has eight levels:
###### A1 : <low, empty>
###### A2 : <scarce, absent>
###### C1 : <or, and>
###### M1 : <may, have to>
###### M2 : <might, must>
###### Q1 : <some, all>
###### Q2 : <most, all>
###### V1 : <try, succeed>

res <- as.data.frame(fread("https://raw.githubusercontent.com/bobvantiel/scalardiversity/master/data/exp2/exp2.txt", 
                           colClasses = c("character", "character", "character", "numeric", "character", 
                                          "character", "numeric", "numeric", "character")))

demo <- as.data.frame(fread("https://raw.githubusercontent.com/bobvantiel/scalardiversity/master/data/exp2/demo2.txt"))

### remove first trials and data from the <may, have to> scale for the no load condition

res <- res[!(res$LOAD == "no" & res$TRIAL == 1),]
res <- res[res$SCALE != "M1",]

### code whether the responses were correct (only for the control condition)

res$CORRECT <- ifelse(res$TYPE == "U", NA,
                      ifelse(res$TYPE == "T" & res$RESP == 1, 1,
                             ifelse(res$TYPE == "F" & res$RESP == 0, 1, 0)))

### determine the errror rate for each participant, and remove participants
### who made errors on more than 20% of the control items or who have a
### native language other than english

demo$CORRECT <- sapply(demo$SUBJECT, function(x) mean(res[res$SUBJECT == x,]$CORRECT, na.rm = TRUE))

bad.subject <- demo[demo$LOAD %in% c("low", "high") & demo$MEMCORRECT < .75,]$SUBJECT
bad.subject <- append(bad.subject, demo[demo$CORRECT < .8,]$SUBJECT)
bad.subject <- append(bad.subject, demo[demo$LANGUAGE %in% c("urdu", "canto", "chinese", "Chinese"),]$SUBJECT)

demo <- demo[!(demo$SUBJECT %in% bad.subject),]
res <- res[!(res$SUBJECT %in% bad.subject),]
res <- res[res$RT < 15000,]
res <- res[res$RT > 200,]

### lineplot showing responses

dfc = summarySE(res, measurevar = "RESP", groupvars = c("SCALE", "TYPE", "LOAD"))

labels <- c(A1 = "Low", A2 = "Scarce", M2 = "Might",
            Q1 = "Some", Q2 = "Most", V1 = "Try", V2 = "Try", C1 = "Or")

dfc$TOFT <- c("C-False", "C-False", "C-False", "C-True", "C-True", "C-True", "Target", "Target", "Target")
dfc$LOADT <- c("High", "Low", "No")
dfc$LOADT <- factor(dfc$LOADT, levels = c("No", "Low", "High"))

mancolor <- brewer.pal(3, "Set1")
 
 pdf("exp2-linegraph.pdf", height = 2.2, width = 8)
 #  pdf("exp2-linegraph.pdf", height = 2.2, width = 8, family = "Linux Libertine")
 ggplot(data = dfc, aes(x = LOADT, y = RESP*100, group = TOFT, color = TOFT)) +
  geom_line(stat = "identity") +
  geom_point() +
  facet_wrap( ~ SCALE, ncol = 7, scales = "free_x", labeller = labeller(SCALE = labels)) +
  theme_linedraw() +
  scale_colour_brewer(palette="Set1", name = "Condition", breaks = c("C-True", "Target", "C-False")) +
  xlab("Memory load") +
  ylab("'True' responses (%)") +
  theme(legend.position = "right",
        legend.key = element_rect(colour = "white"),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(color = "black", size = 11),
        strip.text.y = element_text(color = "black", size = 11),
        strip.background = element_rect(size = 0, fill = "white", colour = "white"),
        axis.title.x = element_text(size = 12, margin = margin(12, 0, 0, 0)),
        axis.title.y = element_text(size = 12, vjust = 0.1, margin = margin(0, 12, 0, 0)),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.margin = margin(0,0,-8,0),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_errorbar(aes(ymin = (RESP*100)-(ci*100), ymax = (RESP*100)+(ci*100)), width = .1) +
  scale_x_discrete(labels = c("No", "Low", "High")) +
  scale_y_continuous(expand = c(0, 2))
 dev.off()

### main analysis: is there an effect of monotonicity?

tar <- res[res$TYPE == "U",]
tar$LOAD.NO <- ifelse(tar$LOAD == "no", 1, ifelse(tar$LOAD == "low", 2, 3))
tar$MON <- ifelse(tar$SCALE %in% c("A1", "A2"), "down", "up")

mon.test <- glmer(RESP ~ ordered(LOAD.NO) * MON + (ordered(LOAD.NO) | SUBJECT) + 
                  (ordered(LOAD.NO) | SCALE), data = tar,
                  family = "binomial", control = glmerControl(optimizer = "bobyqa"))

### analyse memory load for each scalar separately

tar <- res[res$TYPE == "U",]
tar$LOAD.NO <- ifelse(tar$LOAD == "no", 1, ifelse(tar$LOAD == "low", 2, 3))

low.mem    <- glmer(RESP ~ ordered(LOAD.NO) + (1 | SUBJECT), 
                    data = tar[tar$SCALE == "A1",], family = "binomial", control = glmerControl(optimizer = "bobyqa"))
scarce.mem <- glmer(RESP ~ ordered(LOAD.NO) + (1 | SUBJECT), 
                    data = tar[tar$SCALE == "A2",], family = "binomial")
or.mem     <- glmer(RESP ~ ordered(LOAD.NO) + (1 | SUBJECT), 
                    data = tar[tar$SCALE == "C1",], family = "binomial", control = glmerControl(optimizer = "bobyqa"))
might.mem  <- glmer(RESP ~ ordered(LOAD.NO) + (1 | SUBJECT), 
                    data = tar[tar$SCALE == "M2",], family = "binomial", control = glmerControl(optimizer = "bobyqa"))
some.mem   <- glmer(RESP ~ ordered(LOAD.NO) + (1 | SUBJECT), 
                    data = tar[tar$SCALE == "Q1",], family = "binomial")
most.mem   <- glmer(RESP ~ ordered(LOAD.NO) + (1 | SUBJECT), 
                    data = tar[tar$SCALE == "Q2",], family = "binomial")
try.mem    <- glmer(RESP ~ ordered(LOAD.NO) + (1 | SUBJECT),
                    data = tar[tar$SCALE == "V1",], family = "binomial")

### analyse memory load for each pair of scalars

lowscarce.mem <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                       data = tar[tar$SCALE %in%c("A1", "A2"),], family = "binomial")
lowor.mem     <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                       data = tar[tar$SCALE %in%c("A1", "C1"),], family = "binomial")
lowmight.mem  <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                       data = tar[tar$SCALE %in%c("A1", "M2"),], family = "binomial")
lowsome.mem   <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                       data = tar[tar$SCALE %in%c("A1", "Q1"),], family = "binomial",
                       control = glmerControl(optimizer = "bobyqa"))
lowmost.mem   <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                       data = tar[tar$SCALE %in%c("A1", "Q2"),], family = "binomial")
lowtry.mem    <- glmer(RESP ~ ordered(LOAD) * SCALE  + (1 | SUBJECT), 
                       data = tar[tar$SCALE %in%c("A1", "V1"),], family = "binomial")

scarceor.mem    <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                         data = tar[tar$SCALE %in%c("A2", "C1"),], family = "binomial", 
                         control = glmerControl(optimizer = "bobyqa"))
scarcemight.mem <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                         data = tar[tar$SCALE %in%c("A2", "M2"),], family = "binomial")
scarcesome.mem  <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                         data = tar[tar$SCALE %in%c("A2", "Q1"),], family = "binomial", 
                         control = glmerControl(optimizer = "bobyqa"))
scarcemost.mem  <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                         data = tar[tar$SCALE %in%c("A2", "Q2"),], family = "binomial", 
                         control = glmerControl(optimizer = "bobyqa"))
scarcetry.mem   <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                         data = tar[tar$SCALE %in%c("A2", "V1"),], family = "binomial")

ormight.mem <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                     data = tar[tar$SCALE %in%c("C1", "M2"),], family = "binomial", 
                     control = glmerControl(optimizer = "bobyqa"))
orsome.mem  <- glmer(RESP ~ ordered(LOAD) * SCALE + TRIAL + (1 | SUBJECT), 
                     data = tar[tar$SCALE %in%c("C1", "Q1"),], family = "binomial")
ormost.mem  <- glmer(RESP ~ ordered(LOAD) * SCALE + TRIAL + (1 | SUBJECT), 
                     data = tar[tar$SCALE %in%c("C1", "Q2"),], family = "binomial")
ortry.mem   <- glmer(RESP ~ ordered(LOAD) * SCALE + TRIAL + (1 | SUBJECT), 
                     data = tar[tar$SCALE %in%c("C1", "V1"),], family = "binomial")

mightsome.mem <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                       data = tar[tar$SCALE %in%c("M2", "Q1"),], family = "binomial")
mightmost.mem <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                       data = tar[tar$SCALE %in%c("M2", "Q2"),], family = "binomial", 
                       control = glmerControl(optimizer = "bobyqa"))
mighttry.mem  <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                       data = tar[tar$SCALE %in%c("M2", "V1"),], family = "binomial")

somemost.mem <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                      data = tar[tar$SCALE %in%c("Q1", "Q2"),], family = "binomial")
sometry.mem  <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                      data = tar[tar$SCALE %in%c("Q1", "V1"),], family = "binomial", 
                      control = glmerControl(optimizer = "bobyqa"))

mosttry.mem <- glmer(RESP ~ ordered(LOAD) * SCALE + (1 | SUBJECT), 
                     data = tar[tar$SCALE %in%c("Q2", "V1"),], family = "binomial")
