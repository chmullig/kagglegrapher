# Kaggle Leaderboard Confetti Graph

COMP <- "columbia-university-introduction-to-data-science-fall-2012"
#COMP <- "asap-aes"
BIGGER_BETTER <- TRUE

filename <- paste("../leaderboards/", COMP, "_public_leaderboard.csv", sep="")
TITLE <- "Columbia Intro Data Science 2012, Kaggle Competition"
output <- paste(COMP, "confettiboard.png", sep="_")


if (BIGGER_BETTER) {
    best <- max
    cumbest <- cummax
    whichway <- "Score\nHigher is Better ->"
} else {
    best <- min
    cumbest <- cummin
    whichway <- "Score\n<- Lower is Better"
}

#Load the main data
scores <- read.csv(filename)
scores$SubmissionDate <- strptime(scores$SubmissionDate, "%m/%d/%Y %r", tz="UTC")
scores$day <- as.Date(trunc(scores$SubmissionDate, units="days"))
scores <- scores[order(scores$SubmissionDate), ]
scores$besttodate <- cumbest(scores$Score)

mindate <- min(scores$SubmissionDate)
maxdate <- max(scores$SubmissionDate)
xrange <- c(as.Date(mindate), as.Date(maxdate))

if(BIGGER_BETTER) {
    yrange <- c(quantile(scores$Score)[1], max(scores$Score))
} else {
    yrange <- c(min(scores$Score), quantile(scores$Score)[4])
}

dates <- seq(trunc(mindate, units="days"), trunc(maxdate, units="days")+60*60*24, by=60*60*24)
fakedate <- dates[length(dates)]

getbest <- function(Df) c(best=best(Df$Score))
daily_best <- ddply(scores, .(TeamName, day), getbest)
teams <- ddply(scores, .(TeamName), getbest)

scores$teamfac <- factor(scores$TeamName, levels = teams$TeamName[order(teams$best)], ordered=TRUE)

augmented <- daily_best
for (TeamName in unique(scores$TeamName)) {
    currScore <- best(scores$Score[scores$TeamName==TeamName])
    augmented <- rbind(augmented, data.frame(TeamName=TeamName, day=fakedate, V1=currScore))
}

getactivity <- function(Df) c(improvements=nrow(Df), dailybest=best(Df$Score))
activity <- ddply(scores, .(day), getactivity)

activity$besttodate <- cumbest(activity$dailybest)

bestscore <- best(scores$Score)
activity <- rbind(activity, data.frame(day=fakedate, improvements=0, dailybest=0, besttodate=bestscore))

improvements <- scores[scores$besttodate==scores$Score, ]
improvements <- rbind(improvements, improvements[nrow(improvements), ])
improvements[nrow(improvements), ]$SubmissionDate <- maxdate




#daily_max_graph <- ggplot(activity, aes(day, maxtodate)) + geom_step() + scale_x_date(limits=xrange)
#daily_activity_graph <- ggplot(activity, aes(day, V1)) + geom_bar(stat="identity") + scale_x_date(limits=xrange)

#augmented$offleader <- with(merge(augmented, activity, by="day", all=TRUE), V1.x - maxtodate)
#ggplot(augmented, aes(x=day, y=offleader, colour=TeamName)) + geom_step()  + scale_x_date(limits=xrange) + scale_y_log10()
#ggplot(augmented, aes(x=day, y=sqrt(V1), colour=TeamName)) + geom_step()  + scale_x_date(limits=xrange)

png(output, width=900, height=506)
ggplot(improvements, aes(x=as.Date(SubmissionDate), y=Score, colour=teamfac, group=7)) +
    geom_step(size=1.5) +
    scale_x_date(limits=xrange) +
    scale_y_continuous(limits=yrange) +
    geom_point(data=scores, aes(as.Date(SubmissionDate), Score, colour=teamfac, group=teamfac),
               size=1.5) + 
    ggtitle(COMP) + 
    xlab("Submission Date")
dev.off()
