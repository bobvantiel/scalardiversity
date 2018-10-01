### load packages for data import, analysis, and visualisation

library(ggplot2)
library(RColorBrewer)
library(Rmisc)
library(lme4)
library(lmerTest)
library(data.table)
library(Kendall)
# library(extrafont)
# pdfFonts()

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

res <- as.data.frame(fread("https://raw.githubusercontent.com/bobvantiel/scalardiversity/master/data/exp1.txt", 
             colClasses = c("character", "character", "character", "numeric", "character", "character", "numeric", "numeric")))

demo <- as.data.frame(fread("https://raw.githubusercontent.com/bobvantiel/scalardiversity/master/data/demo1.txt"))

### remove first trials and data from the <may, have to> scale, which did not work

res <- res[res$TRIAL != 1,]
res <- res[res$SCALE != "M1",]

### code whether the responses were correct (only for the control condition)

res$CORRECT <- ifelse(res$TYPE == "U", NA,
                     ifelse(res$TYPE == "T" & res$RESP == 1, 1,
                            ifelse(res$TYPE == "F" & res$RESP == 0, 1, 0)))

### determine the errror rate for each participant, and remove participants
### who made errors on more than 20% of the control items

demo$CORRECT <- sapply(demo$SUBJECT, function(x) mean(res[res$SUBJECT == x,]$CORRECT, na.rm = TRUE))

bad.subject <- demo[demo$CORRECT < .80,]$SUBJECT

demo <- demo[!(demo$SUBJECT %in% bad.subject),]
res <- res[!(res$SUBJECT %in% bad.subject),]

### create data file for the response times analyses

correct <- res[res$CORRECT %in% c(1, NA),]
correct <- correct[correct$RT < 15000,]
correct <- correct[correct$RT > 200,]
correct$LOG <- log(correct$RT)
correct$TYPE <- ifelse(correct$TYPE == "F", "T", paste(correct$TYPE))

### barplot showing responses

summary <- summarySE(res, measurevar = "RESP", groupvars = c("SCALE", "TYPE"))
summary$COND <- c("Ctrl-F", "Ctrl-T", "Target")

labels <- c(A1 = "Low", A2 = "Scarce", M2 = "Might", 
            Q1 = "Some", Q2 = "Most", V1 = "Try", C1 = "Or")

pdf("exp1-barplot.pdf", width = 7, height = 1.5)
# pdf("exp1-barplot.pdf", width = 7, height = 1.5, family = "Linux Libertine")
ggplot(data = summary, aes(x = COND, y = RESP*100, fill = COND, colour = COND)) + 
 geom_bar(stat = "identity") + 
 facet_wrap(~ SCALE, ncol = 7, labeller = labeller(SCALE = labels)) + 
 theme_linedraw() + 
 scale_fill_brewer(palette="Set1") + 
 scale_colour_brewer(palette="Set1") + 
 xlab("Condition") + 
 ylab("'True' responses (%)") + 
 theme(
  axis.text.x = element_blank(),
  legend.title = element_blank(),
  axis.ticks.x = element_blank(),
  strip.text.x = element_text(color = "black", size = 11), 
  strip.text.y = element_text(color = "black", size = 11),
  strip.background = element_rect(size = 0, fill = "white", colour = "white"),
  axis.title.x = element_text(size = 11, margin = margin(8, 0, 0, 0)), 
  axis.title.y = element_text(size = 11, vjust = 0.1), 
  axis.text.y = element_text(size = 10),
  panel.border = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  axis.line = element_line(colour = "black")
 ) + 
 geom_errorbar(colour = "black", aes(ymin = (RESP*100)-(se*100), ymax = (RESP*100)+(se*100)), width = .2) + 
 scale_y_continuous(expand = c(0, 0))
dev.off()

### lineplot showing log response times

summary <- summarySEwithin(correct, measurevar = "LOG", 
                           idvar = "SUBJECT", 
                           withinvars = c("SCALE", "TYPE", "RESP"))
summary$COND <- c("Control", "Control", "Target", "Target")

summary$RESP <- as.factor(summary$RESP)

pdf("exp1-decisiontimes.pdf", height = 1.7, width = 7)
# pdf("exp1-decisiontimes.pdf", height = 1.7, width = 7, family = "Linux Libertine")
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

correct$MON <- ifelse(correct$SCALE %in% c("A1", "A2"), "down", "up")

mon.test <- lmer(log(RT) ~ MON * TYPE * RESP + (MON + RESP | SUBJECT) + (MON + RESP | SCALE), data = correct,
                 control = lmerControl(optimizer = "bobyqa"))

### three-way interactions between scalar, condition (target or control), and response (true or false)
### on log response times for each pair of scalars

SoMo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Q1", "Q2"),])
SoOr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Q1", "C1"),])
SoMi <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Q1", "M2"),])
SoTr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Q1", "V1"),])
SoLo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Q1", "A1"),])
SoSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Q1", "A2"),])
MoOr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Q2", "C1"),])
MoMi <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Q2", "M2"),])
MoTr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Q2", "V1"),])
MoLo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Q2", "A1"),])
MoSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("Q2", "A2"),])
OrMi <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("C1", "M2"),])
OrTr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("C1", "V1"),])
OrLo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("C1", "A1"),])
OrSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("C1", "A2"),])
MiTr <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("M2", "V1"),])
MiLo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("M2", "A1"),])
MiSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("M2", "A2"),])
TrLo <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("V1", "A1"),])
TrSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("V1", "A2"),])
LoSc <- lmer(log(RT) ~ SCALE * TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE %in% c("A1", "A2"),])

### analyses of the interaction between condition (target or control) 
### and response (true or false) on log response times

So2 <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE == "Q1",])
Mo2 <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE == "Q2",])
Or2 <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE == "C1",])
Mi2 <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE == "M2",])
Tr2 <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE == "V1",])
Lo2 <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE == "A1",])
Sc2 <- lmer(log(RT) ~ TYPE * RESP + (1 | SUBJECT) + (1 | ITEM), data = correct[correct$SCALE == "A2",])

### analyses of the main effect of response in target trials on log response times

tar <- res[res$TYPE == "U",]

So1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "Q1",])
Mo1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "Q2",])
Or1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "C1",])
Mi1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "M2",])
Tr1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "V1",])
Lo1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "A1",])
Sc1 <- lmer(log(RT) ~ RESP + (1 | SUBJECT) + (1 | ITEM), data = tar[tar$SCALE == "A2",])

### Kendall correlations between number of pragmatic responses to target trials

tar <- res[res$TYPE == "U",]

LOW    <- sapply(unique(tar$SUBJECT), function(x) sum(tar[tar$SUBJECT == x & tar$SCALE == "A1",]$RESP))
SCARCE <- sapply(unique(tar$SUBJECT), function(x) sum(tar[tar$SUBJECT == x & tar$SCALE == "A2",]$RESP))
OR     <- sapply(unique(tar$SUBJECT), function(x) sum(tar[tar$SUBJECT == x & tar$SCALE == "C1",]$RESP))
MIGHT  <- sapply(unique(tar$SUBJECT), function(x) sum(tar[tar$SUBJECT == x & tar$SCALE == "M2",]$RESP))
SOME   <- sapply(unique(tar$SUBJECT), function(x) sum(tar[tar$SUBJECT == x & tar$SCALE == "Q1",]$RESP))
MOST   <- sapply(unique(tar$SUBJECT), function(x) sum(tar[tar$SUBJECT == x & tar$SCALE == "Q2",]$RESP))
TRY    <- sapply(unique(tar$SUBJECT), function(x) sum(tar[tar$SUBJECT == x & tar$SCALE == "V1",]$RESP))

LoScCor <- Kendall(LOW, SCARCE)
LoOrCor <- Kendall(LOW, OR)
LoMiCor <- Kendall(LOW, MIGHT)
LoSoCor <- Kendall(LOW, SOME)
LoMoCor <- Kendall(LOW, MOST)
LoTrCor <- Kendall(LOW, TRY)
ScOrCor <- Kendall(SCARCE, OR)
ScMiCor <- Kendall(SCARCE, MIGHT)
ScSoCor <- Kendall(SCARCE, SOME)
ScMoCor <- Kendall(SCARCE, MOST)
ScTrCor <- Kendall(SCARCE, TRY)
OrMiCor <- Kendall(OR, MIGHT)
OrSoCor <- Kendall(OR, SOME)
OrMoCor <- Kendall(OR, MOST)
OrTrCor <- Kendall(OR, TRY)
MiSoCor <- Kendall(MIGHT, SOME)
MiMoCor <- Kendall(MIGHT, MOST)
MiTrCor <- Kendall(MIGHT, TRY)
SoMoCor <- Kendall(SOME, MOST)
SoTrCor <- Kendall(SOME, TRY)
MoTrCor <- Kendall(MOST, TRY)

### clustering analysis and dendrogram

cl <- t(data.frame(Low = LOW, Scarce = SCARCE, Or = OR, Might = MIGHT, Some = SOME, Most = MOST, Try = TRY))
d <- dist(cl, method = "euclidian")
fit <- hclust(d, method = "ward.D2")
pdf("exp1-cluster.pdf", height = 3.7, width = 4)
#  pdf("exp1-cluster.pdf", height = 3.7, width = 4, family = "Linux Libertine")
plot(fit, hang = 0.1, ylab = "             Height")
dev.off()