#install.packages('tuneR')
#install.packages('seewave')
listen(toto)
getwd()
toto <- readMP3('C:\\Users\\Owner\\Desktop\\africa 1.mp3')
toto <- readMP3('africa 1.mp3')
immun <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\immun.mp3')

str(toto)
class(toto)
summary(toto)
norm1toto <- normalize(toto, unit="1",center=TRUE,level=1)
str(norm1toto)
summary(norm1toto)
summary(toto)
play(toto)
plot(immun)+plot(toto)

class(immun)
summary(immun)
melfcc(immun)
chevy<-readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\chevy.mp3')
str(chevy)
length(chevy)
summary(chevy)
plot(chevy)
length(chevy)
palinL <- mono(palin, "left")
periodogram(palinL, length(palin))
#periodogram(mono(chevy,"left"),length(chevy))
spectrum(palin)
palinWspec <- periodogram(palinL, normalize=TRUE, width=length(palinL), overlap = 0)
palinWspec
summary(palinWspec)
plot(palinWspec)
palinWspec@freq

str(chevyWspec)
length(chevyWspec@freq)

chevyff <- FF(chevyWspec)
summary(chevyff)
str(chevyff)
plot(chevyff)
readWave('toto.wav')
getWavPlayer()
setWavPlayer(' "C:\\Program Files\\Windows Media Player\\wmplayer.exe" ')

#summary(sound.df)
#summary(t(sound.df))

#sound.df2 <- ddply(sound.df, .(Category), summarise, 
 #                  overallmeanL=mean(meanL), overallmeanR=mean(meanR),
  #                 varianceL=mean(varL), varianceR=mean(varR))
#View(sound.df2)

View(music_files[order(music_files$Category),])

all_stats <- NULL
all_stats <- ddply(music_files, .(Category), summarise,
  meanL <- mean(mp3files@left),
  meanR <- mean(mp3files@right),
  varL <- var(mp3files@left),
  varR <- var(mp3files@right),
  sdL <- sd(mp3files@left),
  sdR <- sd(mp3files@right),
  minL <- min(mp3files@left),
  minR <- min(mp3files@right),
  maxL <- max(mp3files@left),
  maxR <- max(mp3files@right),
  rangeL <- maxL-minL,
  rangeR <- maxR-minL)

sound.df3 <- NULL
for (i in 1:length(mp3files)){
  temp <- readMP3(mp3files[i])
  overallstats <- ddply(calc_stats(temp)
  sound.df <- rbind(sound.df, mystats)
  cat("I am",i,"\n")
}

#############################################################################

# (1) tables (2) wave plots (3) graphs
# parallel coordinate plot, color by category
# plots
# look for different genders, number of speakers, etc.
# count number of words, number of speakers, put in table
# use plots and label with # speakers and gender
# have audience match mystery wave with correct category visually (then quantitatively?)

#install.packages("gridExtra")
#install.packages("xtable")
#install.packages("iplots")

library(gridExtra)
library(tuneR)
library(seewave)
library(ggplot2)
library(plyr)
library(xtable)
library(iplots)
setwd('C:\\Users\\Owner\\Documents\\GitHub\\585_Project')

mp3files <- list.files(path='C:\\Users\\Owner\\Documents\\GitHub\\585_Project', pattern='*.mp3')
mp3files

calc_stats <- function(x){
  name <- mp3files[i]
  meanL <- mean(x@left)
  meanR <- mean(x@right)
  varL <- var(x@left)
  varR <- var(x@right)
  sdL <- sd(x@left)
  sdR <- sd(x@right)
  minL <- min(x@left)
  minR <- min(x@right)
  maxL <- max(x@left)
  maxR <- max(x@right)
  rangeL <- abs(maxL-minL)
  rangeR <- abs(maxR-minL)
  #outliersL <- something
  #outliersR <- something
  #periodsL <- periodogram(x@left, width=length(x))
  #periodsR <- periodogram(x@right, width=length(x))
  df <- data.frame(name, meanL, meanR, varL, varR, sdL, sdR, minL, minR, maxL, maxR, rangeL, rangeR)
  return(df)
}

sound.df <- NULL
for (i in 1:length(mp3files)){
  temp <- readMP3(mp3files[i])
  mystats <- calc_stats(temp)
  sound.df <- rbind(sound.df, mystats)
  cat("I am",i,"\n")
}

sound.df$Category[sound.df$name %in% c("xbox.mp3","oxi.mp3","pest.mp3","snore.mp3","pjs.mp3")] <- "Commercial"
sound.df$Category[sound.df$name %in% c("math.mp3","engl.mp3","law.mp3","stat.mp3","hist.mp3")] <- "Lecture"
sound.df$Category[sound.df$name %in% c("foxnews.mp3","khou.mp3","nbc.mp3","cnn.mp3")] <- "News report"
sound.df$Category[sound.df$name %in% c("obama.mp3","palin.mp3","lbj.mp3","bush.mp3","carter.mp3")] <- "Political speech"
sound.df$Category[sound.df$name %in% c("origami.mp3","bball.mp3","firstaid.mp3","makeup.mp3","uke.mp3")] <- "How to"
sound.df$Category[sound.df$name %in% c("slam1.mp3","slam2.mp3","slam3.mp3","slam4.mp3","slam5.mp3","slam6.mp3")] <- "Slam poetry"

View(sound.df[order(sound.df$Category),])

calc_stats2 <- function(x){
  name <- mp3files[i]
  xWspecL <- periodogram(mono(x,"left"), normalize=TRUE, width=length(mono(x,"left")), overlap=0)
  xWspecR <- periodogram(mono(x,"right"), normalize=TRUE, width=length(mono(x,"right")), overlap=0)
  freqL <- length(xWspecL@freq)
  num_periodsL <- length(xWspecL@spec)
  widthL <- xWspecL@width
  overlapL <- xWspecL@overlap
  samp_rateL <- xWspecL@samp.rate
  varL <- xWspecL@variance
  energyL <- xWspecL@energy
  #freqR <- length(xWspecR@freq)
  #num_periodsR <- length(xWspecR@spec)
  #widthR <- xWspecR@width
  #overlapR <- xWspecR@overlap
  #samp_rateR <- xWspecR@samp.rate
  varR <- xWspecR@variance
  energyR <- xWspecR@energy
  df2 <- data.frame(name, freqL, num_periodsL, widthL, 
                    overlapL, samp_rateL, varL, varR, energyL, energyR)
  return(df2)
}

sound.df2 <- NULL
for (i in 1:length(mp3files)){
  temp <- readMP3(mp3files[i])
  mystats2 <- calc_stats2(temp)
  sound.df2 <- rbind(sound.df2, mystats2)
  cat("I am",i,"\n")
}

sound.df2

qplot(data=sound.df2, x=name, y=energyL)

qplot(data=sound.df, x=Category, y=meanL)

qplot(data=range.df, x=Category, y=avg_range)
range.df <- ddply(sound.df, .(Category), summarise,
                  avg_range=mean((rangeL+rangeR)/2))

attach(sound.df)
par(mfrow=c(2,2))
boxplot(Category, meanL)
boxplot(Category, meanR)
boxplot(Category, varL)
boxplot(Category, varR)

qplot(data=sound.df, x=Category, y=varL)
qplot(data=sound.df, x=Category, y=rangeL)
qplot(data=sound.df, x=Category, y=meanL, geom="boxplot")
qplot(data=sound.df, x=Category, y=meanL, color=Category)
library(ggplot2)

foxnews <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\foxnews.mp3')
khou <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\khou.mp3')
nbc <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\nbc.mp3')
cnn <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\cnn.mp3')

slam1 <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\slam1.mp3')
slam2 <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\slam2.mp3')
slam3 <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\slam3.mp3')
slam4 <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\slam4.mp3')
slam5 <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\slam5.mp3')
slam6 <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\slam6.mp3')

origami <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\origami.mp3')
firstaid <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\firstaid.mp3')
bball <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\bball.mp3')
makeup <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\makeup.mp3')
uke <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\uke.mp3')

obama <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\obama.mp3')
palin <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\palin.mp3')
lbj <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\lbj.mp3')
bush <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\bush.mp3')
carter <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\carter.mp3')
par(mfrow=c(3,2))
plot(mono(obama,"left"))
plot(mono(palin,"left"))
plot(mono(lbj,"left"))
plot(mono(bush,"left"))
plot(mono(carter,"left"))

math <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\math.mp3')
engl <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\engl.mp3')
stat <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\stat.mp3')
law <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\law.mp3')
#hist <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\hist.mp3')

xbox <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\xbox.mp3')
oxi <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\oxi.mp3')
pjs <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\pjs.mp3')
pest <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\pest.mp3')
snore <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\snore.mp3')

par(mfrow=c(3,2))
plot(mono(nbc,"left"))
plot(mono(slam,"left"))
plot(mono(uke,"left"))
plot(mono(bush,"left"))
plot(mono(stat,"left"))
plot(mono(pjs,"left"))

par(mfrow=c(2,1))
plot(mono(slam3,"left"),main="Poet 3")
plot(mono(slam4,"left"),main="Poet 4")

d <- dist(sound.df, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=6) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=6, border="red")

music_files <- NULL
music_files <- as.data.frame(mp3files)
music_files
str(music_files)

music_files$Category[music_files$mp3files %in% c("xbox.mp3","oxi.mp3","pest.mp3","snore.mp3","pjs.mp3")] <- "Commercial"
music_files$Category[music_files$mp3files %in% c("math.mp3","engl.mp3","law.mp3","stat.mp3","hist.mp3")] <- "Lecture"
music_files$Category[music_files$mp3files %in% c("foxnews.mp3","khou.mp3","nbc.mp3","cnn.mp3")] <- "News report"
music_files$Category[music_files$mp3files %in% c("obama.mp3","palin.mp3","lbj.mp3","bush.mp3","carter.mp3")] <- "Political speech"
music_files$Category[music_files$mp3files %in% c("origami.mp3","bball.mp3","firstaid.mp3","makeup.mp3","uke.mp3")] <- "How to"
music_files$Category[music_files$mp3files %in% c("slam1.mp3","slam2.mp3","slam3.mp3","slam4.mp3","slam5.mp3","slam6.mp3")] <- "Slam poetry"

ipcp(sound.df[c("meanL","varL","maxL")])
identify(labels=row.names(sound.df))
attach(sound.df)
plot(names, meanL) # scatterplot
identify(names, meanL, labels=row.names(sound.df)) # identify points 
coords <- locator(type="l") # add lines
coords # display list

par(mfrow=c(5,1))
plot(mono(slam1,"left"))
plot(mono(slam2,"left"))
plot(mono(slam3,"left"))
plot(mono(slam4,"left"))
plot(mono(slam5,"left"))
plot(mono(slam1,"left"))
plot(mono(cnn,"left"))

p <- ggpcp(sound.df)
p
p + geom_line(aes(colour=sound.df$Category))
ggparallel(list("meanL", "maxL","varL"), data=sound.df, method="parset",label=TRUE)

install.packages("ggparallel")
library(ggparallel)
library(devtools)
install.packages("slidify")
library(slidify)

require(ggplot2)
require(reshape2)
require(reshape)

df_m <- melt(reshape:::rescaler(sound.df), id.vars=c('name', 'Category'), measure.vars=c('meanL','maxL','sdL'))
View(df_m)
ggplot(df_m) + geom_line(aes(x=variable,y=value,group=name,color=Category),size=I(1.1)) + geom_text(aes(x=variable,y=value,label=name),hjust=0,vjust=0,angle=45)

View(xtable(sound.df))