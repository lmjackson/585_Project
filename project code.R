install.packages('tuneR')

library(tuneR)
install.packages('seewave')
library(seewave)
listen(toto)
getwd()
setwd('C:\\Users\\Owner\\Documents\\GitHub\\585_Project')
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
summary(chevy)
plot(chevy)
length(chevy)
periodogram(chevyL, width=3250944)
spectrum(chevy)
chevyL <- mono(chevy, "left")
chevyWspec <- periodogram(chevyL, normalize=TRUE, width=length(chevyL), overlap = 0)
chevyWspec
summary(chevyWspec)
plot(chevyWspec, xlim=c(-2000,2000), which=1)

chevyff <- FF(chevyWspec)
plot(chevyff)
readWave('toto.wav')
getWavPlayer()
setWavPlayer(' "C:\\Program Files\\Windows Media Player\\wmplayer.exe" ')

chevy <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\chevy.mp3')
foxnews <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\foxnews.mp3')
obama <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\obama.mp3')
origami <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\origami.mp3')
slam1 <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\slam1.mp3')
theview <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\theview.mp3')
immun <- readMP3('C:\\Users\\Owner\\Documents\\GitHub\\585_Project\\immun.mp3')

q1(immun)

mp3files <- list.files(path='C:\\Users\\Owner\\Documents\\GitHub\\585_Project', pattern='*.mp3')
View(mp3files)


dataframe <- function(mp3files){
  d.frame <- data.frame()
for (i in 1:length(mp3files)){
d.frame <- rbind(d.frame, readMP3(mp3files[[i]]))
}
}

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
 rangeL <- maxL-minL
 rangeR <- maxR-maxL
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

sound.df$Category[sound.df$name %in% c("chevy.mp3")] <- "Commercial"
sound.df$Category[sound.df$name %in% c("math.mp3","engl.mp3","psyc.mp3")] <- "Lecture"
sound.df$Category[sound.df$name %in% c("foxnews.mp3")] <- "News report"
sound.df$Category[sound.df$name %in% c("obama.mp3")] <- "Political speech"
sound.df$Category[sound.df$name %in% c("origami.mp3")] <- "How to"
sound.df$Category[sound.df$name %in% c("slam1.mp3","slam2.mp3","slam3.mp3")] <- "Slam poetry"

sound.df[order(sound.df$Category),]



