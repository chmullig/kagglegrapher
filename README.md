# KaggleGrapher

Python + R workflow to build a graph of a Kaggle competition leaderboard.
See `datascience_leaderboard.png` for an example.

## What it does
The python script `leaderGrabber.py` downloads the Kaggle raw leaderboard data 
described in `config.py` and saves the CSV out to disk. It then calls the R
script `makeGraph.R`, which must be configured with some basic parameters (score
range, min/max date, file names, title), which creates the graph and saves a png.
`leaderGrabber` then uses sips to resize the image, if there are any sizes 
defined in config, for use as thumbnails, etc. It then scps it to a destination
of your choice.

## Requirements
As a somewhat adhoc system this is a bit ugly. It uses Python and R, and seems
to require a reasonably modern R (I've only tested it on Python 2.7 and R 2.15
on OS X).

The only non-standard dependency is that python uses Mechanize to automatically
download the CSV, because Kaggle wanted me to login.

