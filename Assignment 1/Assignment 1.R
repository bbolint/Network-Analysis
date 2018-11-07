#Q1
#a,
a <- c(8, NA, 20, 5, 7, 9)
mean(a)

mean(a,na.rm = T)

#b,
a <- a[-2]
mean(a)

#Q2
#a,
matrix_Q2 <- matrix(c(0,1,1,1,rep(0,12)),4,4,byrow = T)

#b,
library(qgraph)
qgraph(matrix_Q2)

#Q3,
#a,
library(psych)
# Load bfi dataset:
data(bfi)
# Look at data:
View(bfi)

#b,
sum(bfi$gender==1)
sum(bfi$gender==2)

#c,
mean(bfi$age)

#d,
data_Q3 <- bfi[,1:25]
fa_Q3 <- fa(data_Q3, rotate = 'promax')

#e,
qgraph.loadings(unclass(fa_Q3$loadings))

#Q4,
#b,
library(foreign)
data_Q4 <- read.spss('job_performance.sav',to.data.frame = T)

#c,
cor(data_Q4[,-1])

#d,
fit <- lm(perf ~ iq + mot + soc, data = data_Q4)

#e,
summary(fit)

#Q5,
#a,
data_Q5 <- read.spss('PTSDdata.sav',to.data.frame = T)
data_Q5 <- data_Q5[,10:30]

#b,
colnames(data_Q5) <- c('Lifetime Trauma',
                       'Intrusive thoughts',
                       'Nightmares',
                       'Flashbacks',
                       'Emotional cue reactivity',
                       'Psychological cue reactivity',
                       'Avoidance of thoughts',
                       'Avoidance of reminders',
                       'Trauma-related amnesia',
                       'Negative beliefs',
                       'Blame of self or others',
                       'Negative trauma-related emotions',
                       'Loss of interest',
                       'Detachment',
                       'Restricted affect',
                       'Irritability / anger',
                       'Self-destructive / reckless behaviour',
                       'Hypervigilance',
                       'Exaggerated startle response',
                       'Difficulty concentrating',
                       'Sleep disturbance')
head(data_Q5)

#c,
round(cor(data_Q5[,-1]),2)

#d,
sum(data_Q5$Nightmares)
sum(data_Q5$`Sleep disturbance`)

#e,
cor.test(data_Q5$`Lifetime Trauma`,data_Q5$Nightmares)

#f,
library(ppcor)
pcor.test(data_Q5$`Lifetime Trauma`,data_Q5$Nightmares,data_Q5$`Negative trauma-related emotions`)

#Bonus Q,
data_bonus <- read.csv('studentdata.csv',sep=';')

par(mfrow = c(1,1), mar = c(20,5,10,5))


barplot(prop.table(table(data_bonus$What.is.your.gender.)),  col=rgb(0,0,1,1/4),
        main = 'Gender', las = 2, cex.axis=1)

barplot(prop.table(table(data_bonus$I.am.a.student.in.the.)),  col=rgb(0,0,1,1/4),
        main = 'Study programme', las = 2)

barplot(prop.table(table(data_bonus$How.familiar.are.you.with.R.)),  col=rgb(0,0,1,1/4),
        main = 'Familiarity with R', las = 2)

barplot(prop.table(table(data_bonus$What.is..are..your.specialization.s..)),  col=rgb(0,0,1,1/4),
        main = 'Specialization', las = 2)

barplot(prop.table(table(data_bonus$Where.are.you.from.)),  col=rgb(0,0,1,1/4),
        main = 'Home country', las = 2)









