import selenium
from selenium import webdriver
import os
#os.getcwd()
#os.chdir('C:/Users/Caio/repos/nba-models')

# start browser crawler
browser = webdriver.Firefox()

## GRAB SEASON PLAYER STAT PER GAME
for season in range(1976, 2018):
    # navigate to bballref
    url = 'https://www.basketball-reference.com/leagues/NBA_' + str(season) + '_per_game.html'
    browser.get(url)
    # activate and grab data in CSV format
    raw_csv = browser.execute_script('''document.getElementById("per_game_stats_toggle_partial_table").click();
    var x = document.getElementsByClassName("tooltip");
    x[3].click();
    var content = document.getElementById("csv_per_game_stats")
    return content.textContent
    ''')

    # write to CSV
    pathstr = "season-stats-pergame/" + str(season) + ".csv"
    f = open(pathstr, "w")
    f.write(raw_csv)

## GRAB SEASON PLAYER STAT TOTALS
for season in range(1976, 2018):
    season=1977
    # navigate to bballref
    url = 'https://www.basketball-reference.com/leagues/NBA_' + str(season) + '_totals.html'
    browser.get(url)
    # activate and grab data in CSV format
    raw_csv = browser.execute_script('''document.getElementById("totals_stats_toggle_partial_table").click();
    var x = document.getElementsByClassName("tooltip");
    x[3].click();
    var content = document.getElementById("csv_totals_stats")
    return content.textContent
    ''')

    # write to CSV
    pathstr = "season-stats-totals/" + str(season) + ".csv"
    f = open(pathstr, "w")
    f.write(raw_csv)

## GRAB SEASON MVP STATS
for season in range(1976, 1977):
    # navigate to bballref
    url = 'https://www.basketball-reference.com/awards/awards_' + str(season) + '.html'
    browser.get(url)
    # activate and grab data in CSV format
    raw_csv = browser.execute_script('''var x = document.getElementsByClassName("tooltip");
    x[3].click();
    var content = document.getElementById("csv_mvp")
    if (content==null) {
        var content = document.getElementById("csv_nba_mvp")
    }
    return content.textContent
    ''')

    # write to CSV
    raw_csv = raw_csv[133:]
    pathstr = "award-stats/" + str(season) + ".csv"
    f = open(pathstr, "w")
    f.write(raw_csv)
