#!/usr/bin/env python
import mechanize
from subprocess import call
import os, sys
import csv
from collections import Counter
import operator

from config import *
#expect to find the following defined in config: 
# email, password,

TMP_FILENAME = "USER_DOWNLOADER.status"

def generate_list_of_users(team_file):
    print '   -> Loading team data...'
    team_csv = csv.reader(open(team_file, "rU"))
    user_counts = Counter()
    header = team_csv.next()
    for row in team_csv:
        user_counts[int(row[1])] += 1
    users = [x[0] for x in
        sorted(user_counts.items(), key=operator.itemgetter(1), reverse=True)]
    print user_counts.most_common(10)

    #Let's check if we terminated before we were done, and if so continue from there
    last_id = 0
    try:
        tmp_file = open(TMP_FILENAME)
        last_id = int(tmp_file.read().strip())
    except:
        # don't appear to have been interrupted, so we'll start from zero....
        pass

    if last_id:
        print "      -> Continuing at user %s" % last_id
        last_good = users.index(last_id)
        users = users[last_good + 1:]
    else:
        print '   -> Starting from scratch'
    return users


def download_profiles(users):
    print '   -> Starting mechanize browser'
    br = mechanize.Browser()

    print '   -> Beginning to fetch users...'
    total = len(users)
    tmp_file = open(TMP_FILENAME, 'w')
    for i, user in enumerate(users):
        url = "http://www.kaggle.com/users/%s" % user
        try:
             r = br.open(url)
        except:
            sys.stdout.write("\r      Fuck, user %s doesn't work. Skipping..."
                % user)
            continue

        profile_page = br.response().read()
        output_filename = os.path.join("users", "%s.html" % user)
        profile = open(output_filename, "w")
        profile.write(profile_page)
        profile.close()
        if not i % 100:
            tmp_file.seek(0)
            tmp_file.truncate()
            tmp_file.write(str(user))
            tmp_file.flush()
            sys.stdout.write("\r      -> User %s  (%s of %s; %.1f%%)            "
                % (user, i, total, float(i)/total))
            sys.stdout.flush()


    total.close()
    os.remove(TMP_FILENAME)
    print '   -> Downloaded all users'

if __name__ == '__main__':
    if not os.path.exists("users"):
        os.mkdir("users")
    users = generate_list_of_users(sys.argv[1])
    download_profiles(users)
    print '   -> Done'
