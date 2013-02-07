#!/usr/bin/env python
import mechanize
import zipfile
from subprocess import call

from config import *
#expects to find the following defined in config: 
# email, password,
# url (eg = "http://inclass.kaggle.com/c/columbia-university-introduction-to-data-science-fall-2012/publicleaderboarddata.zip"),
# filename (eg = "columbia-university-introduction-to-data-science-fall-2012_public_leaderboard.csv")
# output (eg "datascience_leaderboard") (becomes "datascience_leaderboard.png")
# sizes (eg [640, 150]) for automated resizing. may be empty.

def download_leaderboard():
    print '   -> Starting mechanize browser'
    br = mechanize.Browser()
    br.open("http://kaggle.com/account/login")

    br.select_form(nr=0)
    br["UserName"] = email
    br["Password"] = password
    loginResponse = br.submit()

    #download the leaderboard zip
    ltempfile, headers = br.retrieve(url)

    #extract the CSV
    lzip = zipfile.ZipFile(ltempfile)
    csv = open(filename, "w")
    csv.write(lzip.read(filename))
    csv.close()
    print '   -> Downloaded'

def makegraph():
    print '   -> creating graph with R'
    r = call(["Rscript", "makegraph.R"])
    if sizes:
        print '   -> Resizing'
        for size in sizes:
            resize = call(["sips", "--out", "%s-%sx%s.png" % (output, size, size), "-Z", "%s" % size, "%s.png" % output])
    print '   -> Graphs ready'

def uploadgraph():
    print '   -> Uploading graph'

    scp = call(['scp', "%s.png" % output,]
                    + ["%s-%sx%s.png" % (output, size, size) for size in sizes]
                    + ['chmullig.com:webapps/chmullig/wp-content/uploads/2012/11/'])


if __name__ == '__main__':
    download_leaderboard()
    makegraph()
    uploadgraph()
    print '   -> Done'
