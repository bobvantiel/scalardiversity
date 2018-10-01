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
###### Lo : <low, empty>
###### Sc : <scarce, absent>
###### Or : <or, and>
###### Mi : <might, must>
###### So : <some, all>
###### Mo : <most, all>
###### Tr : <try, succeed>

res <- as.data.frame(fread("https://raw.githubusercontent.com/bobvantiel/scalardiversity/master/data/exp3.txt", 
                           colClasses = c("character", "character", "character", "numeric", 
                                          "character", "character", "character", "character",
                                          "numeric", "numeric")))

demo <- as.data.frame(fread("https://raw.githubusercontent.com/bobvantiel/scalardiversity/master/data/demo3.txt"))

### code whether the responses were correct (only for the control condition)

res$CORRECT <- ifelse(res$TYPE == "U" & res$TRAINING == "logical" & res$RESP == 1, 1,
                      ifelse(res$TYPE == "U" & res$TRAINING == "pragmatic" & res$RESP == 0, 1,
                             ifelse(res$TYPE == "T" & res$RESP == 1, 1,
                                    ifelse(res$TYPE == "F" & res$RESP == 0, 1, 0))))

### determine the errror rate for each participant, and remove participants
### who made errors on more than 20% of the control items

demo$CORRECT <- sapply(demo$SUBJECT, function(x) mean(res[res$SUBJECT == x,]$CORRECT, na.rm = TRUE))

bad.subject <- demo[demo$CORRECT < .80,]$SUBJECT
bad.subject <- append(bad.subject, 
                      demo[demo$LANGUAGE %in% c("Japanese", "Chinese", "Russian", "Turkish", 
                                                "bulgarian", "SPANISH", "German", "Spanish"),]$SUBJECT)

demo <- demo[!(demo$SUBJECT %in% bad.subject),]
res <- res[!(res$SUBJECT %in% bad.subject),]

### create data file for the response times analyses

correct <- res[res$CORRECT %in% c(1, NA),]
correct <- correct[correct$RT < 15000,]
correct <- correct[correct$RT > 200,]
correct$LOG <- log(correct$RT)
correct$TYPE <- ifelse(correct$TYPE == "F", "T", paste(correct$TYPE))

### lineplot showing log response times

summary <- summarySEwithin(correct, measurevar = "LOG", 
                           idvar = "SUBJECT", 
                           withinvars = c("SCALE", "TYPE", "RESP"))
summary$COND <- c("Control", "Control", "Target", "Target")
summary$SCALE <- factor(summary$SCALE, levels = c("Lo", "Sc", "Or", "Mi", "So", "Mo", "Tr"))

labels <- c(Lo = "Low", Sc = "Scarce", Mi = "Might", 
            So = "Some", Mo = "Most", Tr = "Try", Or = "Or")

pdf("exp3-decisiontimes.pdf", height = 1.7, width = 7)
# pdf("exp3-decisiontimes.pdf", height = 1.7, width = 7, family = "Linux Libertine")
ggplot(data = summary, aes(x = RESP, y = LOG, group = COND, colour = COND, shape = COND)) +
 geom_line(aes(linetype = COND)) +
 geom_point() +
 scale_x_discrete(limits=c("1", "0"), labels = c("True", "False"), name = "Response", expand = c(0.3, 0)) +
 scale_y_continuous(name = "Log response time") +
 theme_linedraw() +
 theme(legend.key = element_rect(colour = "white"),
       legend.key.size = unit(0.5, "cm"),
       legend.title = element_text(size = 10),
       legend.text = element_text(size = 10),
       strip.text.x = element_text(color = "black", size = 11),
       strip.text.y = element_text(color = "black", size = 11),
       strip.background = element_rect(size = 0, fill = "white", colour = "white"),
       axis.title.x = element_text(size = 11, margin = margin(8, 0, 0, 0)),
       axis.title.y = element_text(size = 11, vjust = 0.1, margin = margin(0, 12, 0, 0)),
       axis.text.x = element_text(size = 10),
       axis.text.y = element_text(size = 10),
       panel.border = element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       axis.line = element_line(colour = "black")) +
 geom_errorbar(aes(ymin = LOG-ci, ymax = LOG+ci), width = .1) +
 scale_colour_brewer(palette="Set1", name = "Condition") +
 scale_linetype_manual(values = c("solid", "longdash"), name = "Condition") +
 scale_shape(name = "Condition") +
 facet_wrap( ~ SCALE, ncol = 7, scales = "free_x", labeller = labeller(SCALE = labels))
dev.off()

### main analysis: is there an effect of monotonicity?

correct$MON <- ifelse(correct$SCALE %in% c("Lo", "Sc"), "down", "up")

mon.test <- lmer(log(RT) ~ MON * TYPE * RESP + (MON + TYPE + RESP | SUBJECT), data = correct,
                 control = lmerControl(optimizer = "bobyqa"))

### three-way interactions between scalar, condition (target or control), and response (true or false)
### on log response times for each pair of scalars

SoMo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("So", "Mo"),])
SoOr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("So", "Or"),])
SoMi <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("So", "Mi"),])
SoTr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("So", "Tr"),])
SoLo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("So", "Lo"),])
SoSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("So", "Sc"),])
MoOr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Mo", "Or"),])
MoMi <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Mo", "Mi"),])
MoTr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Mo", "Tr"),])
MoLo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Mo", "Lo"),])
MoSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Mo", "Sc"),])
OrMi <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Or", "Mi"),])
OrTr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Or", "Tr"),])
OrLo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Or", "Lo"),])
OrSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Or", "Sc"),])
MiTr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Mi", "Tr"),])
MiLo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Mi", "Lo"),])
MiSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Mi", "Sc"),])
TrLo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Tr", "Lo"),])
TrSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Tr", "Sc"),])
LoSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Lo", "Sc"),])

### analyse response times for each scalar separately

scarce.rt <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT), data = correct[correct$SCALE == "Sc",])
some.rt   <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT), data = correct[correct$SCALE == "So",])
low.rt    <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT), data = correct[correct$SCALE == "Lo",])
might.rt  <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT), data = correct[correct$SCALE == "Mi",])
or.rt     <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT), data = correct[correct$SCALE == "Or",])
try.rt    <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT), data = correct[correct$SCALE == "Tr",])
most.rt   <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT), data = correct[correct$SCALE == "Mo",])


### analyses of the main effect of response in target trials on log response times

tar <- correct[correct$TYPE == "U",]

So1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "So",])
Mo1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "Mo",])
Or1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "Or",])
Mi1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "Mi",])
Tr1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "Tr",])
Lo1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "Lo",])
Sc1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "Sc",])

