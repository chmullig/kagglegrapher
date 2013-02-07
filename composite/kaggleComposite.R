#Generates a graph based on Kaggle competition CSV
#Should be called from (or at least after) leaderGrabber.py, which downloads the data
#Intelligently tries to figure out coloring, and range, once you configure
#the constants.

#Constants. Must agree with config.py.
COMP = "columbia-university-introduction-to-data-science-fall-2012"
filename <- paste("../leaderboards/", COMP, "_public_leaderboard.csv", sep="")
TITLE <- "Columbia Intro Data Science 2012, Kaggle Competition"
OUTPUT <- paste(COMP, "leaderboard.png", sep="_")

#Load the main data
scores <- read.csv(filename)
scores$SubmissionDate <- strptime(scores$SubmissionDate, "%m/%d/%Y %r", tz="UTC")
scores$day <- as.Date(trunc(scores$SubmissionDate, units="days"))
scores <- scores[order(scores$SubmissionDate), ]

mindate <- as.Date(min(scores$SubmissionDate))
maxdate <- as.Date(max(scores$SubmissionDate))
xrange <- c(mindate, maxdate)

yrange <- c(min(scores$Score), max(scores$Score))



dates <- seq(trunc(mindate, units="days"), trunc(maxdate, units="days")+60*60*24, by=60*60*24)
teams <- unique(scores$TeamName)
getbest <- function(Df) max(Df$Score)
daily_best <- ddply(scores, .(TeamName, day), getbest)

getactivity <- function(Df) c(nrow(Df), max(Df$Score))
activity <- ddply(scores, .(day), getactivity)
activity$maxtodate <- cummax(activity$V2)

daily_max_graph <- ggplot(activity, aes(day, maxtodate)) + geom_step() + scale_x_date(limits=c(mindate, maxdate))
daily_activity_graph <- ggplot(activity, aes(day, V1)) + geom_bar(stat="identity") + scale_x_date(limits=c(mindate, maxdate))

augmented <- daily_best
for (TeamName in unique(scores$TeamName)) {
    currScore <- max(scores$Score[scores$TeamName==TeamName])
    augmented <- rbind(augmented, c(TeamName, as.POSIXlt(dates[length(dates)]), currScore))
}





#Ensure the text size and trim length are set properly to show as much name as possible
TEXTSIZE <- .75
MAXLEN <- 16/TEXTSIZE
adjustmentfactor=(yrange[2]-yrange[1])*.015
adjustmentpadding=adjustmentfactor*.005
palette(c("#E41A1C", "purple", "#A6D854", "#A6761D", "orange", "#377EB8", 
"#FF00AA", "#1B9E77", "turquoise", "#66A61E", "blue", 
"red", "forest green", "#FC8D62", "orange",
"#7570B3", "#E78AC3", "#CF0234", "#1B9E77", "#66A61E", 
"#D95F02", "#E6AB02", "blue"))
colors <- palette()

#need to check if this lower or higher is better...
#If lower is better let's set 

#Make sure the final labels will be sufficiently spread out
#This finds any points that are close together than adjustmentfactor times the
#text size And moves the top one up, and the bottom one down by about half the
#space Required to make them adjustmentfactor*TEXTSIZE apart. It movies the top
#one up slightly more because that was more aesthetically pleasing
bests <- aggregate(scores$Score, list(TeamName = scores$TeamName), max)
bests <- bests[order(bests$x), ]
badPoints <- which(diff(bests$x) < adjustmentfactor*TEXTSIZE)
i <- 0
while (length(badPoints) > 0)
  {
  bests$x[badPoints] <- bests$x[badPoints] - (((adjustmentfactor * TEXTSIZE + adjustmentpadding) - diff(bests$x)[badPoints])*0.5)
  bests$x[badPoints+1] <- bests$x[badPoints+1] + (((adjustmentfactor * TEXTSIZE + adjustmentpadding) - diff(bests$x)[badPoints])*0.5)
  badPoints <- which(diff(bests$x) < adjustmentfactor*TEXTSIZE)
  i <- i + 1
  }
nTeams <- nrow(bests)
print(paste("Spreading required", i, "iterations"))

#png(filename=OUTPUT, width=1024, height=1024)

#Setup the plot, title, axis labels, etc
par(mar=par()$mar+c(0,0,0,6),bty="l",yaxs="i", xaxs="i")
plot(xrange, yrange, type="n", xaxt='n', xlab="Submission Time", ylab="Score", main=TITLE)
atx <- seq(mindate, maxdate, by=(maxdate-mindate)/6)
axis(1, at=atx, labels=format(atx, "%b\n%d"), padj=0.5)
mtext(side=3, text=paste(nrow(scores), "submitted improvements by", nTeams, "teams as of", format(maxdate, format="%B %d %Y %l:%M %p")))
mtext(side=4, text="Team & Current Score", at=yrange[2], las=2, line=-0.5)

colori = 1

#For each TeamName plot their Scores and label in the margin
for (TeamName in unique(scores$TeamName)) {
    i = which(unique(scores$TeamName)==TeamName)
    currScore <- max(scores$Score[scores$TeamName==TeamName])
    xvals <- scores$SubmissionDate[scores$TeamName==TeamName]
    yvals <- scores$Score[scores$TeamName==TeamName]
	#These next two lines add a datapoint for their current Score right now
    xvals <- append(xvals, maxdate)
    yvals <- append(yvals, currScore)
    
    #if benchmark, black, they stayed still they'll be grey, otherwise we get another color
    if (grepl("Benchmark", TeamName, ignore.case=TRUE)) { 
      color <- "black"
    } else if (min(yvals) == max(yvals)) {
        color <- "gray40"
    } else {
        color <- colors[colori]
        colori <- colori + 1
    }
    
    lines(xvals, yvals, col=color, lwd=2, type="s")
    displayName <- TeamName
    #Trim the TeamName name if it's too long to be shown
    if (nchar(as.character(displayName)) > MAXLEN) {
        displayName <- paste(substring(displayName, 0, MAXLEN-4), "...", sep="")
    }
    mtext(side=4, at=bests$x[bests$TeamName==TeamName], text=paste(displayName, round(currScore, 3)), col=color, line=0.5, las=2, cex=TEXTSIZE)
    print(paste(TeamName, color))
}

#dev.off()
