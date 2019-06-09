import os, time
import selenium
from selenium import webdriver
#os.getcwd()
#os.chdir('C:/Users/Caio/repos/nba-models')

# start browser crawler
browser = webdriver.Firefox()

# grab season player stat per game
scrapeBbalRef(2018, 2018, 'https://www.basketball-reference.com/leagues/NBA_*SEASON*_per_game.html','per_game_stats','season-stats-pergame')
# grab season player stat totals
scrapeBbalRef(2018, 2018, 'https://www.basketball-reference.com/leagues/NBA_*SEASON*_totals.html','totals_stats','season-stats-totals')
# grab season advanced player stats
scrapeBbalRef(1997, 1997, 'https://www.basketball-reference.com/leagues/NBA_*SEASON*_advanced.html','advanced_stats','season-stats-advanced')
# grab MVP stats
scrapeBbalRef(2018, 2018, 'https://www.basketball-reference.com/awards/awards_*SEASON*.html','nba_mvp','award-stats', False)
# grab standings stats
scrapeBbalRef(2018, 2018, 'https://www.basketball-reference.com/leagues/NBA_*SEASON*_standings.html', 'expanded_standings','season-standings', False)

def scrapeBbalRef(year_start, year_end, page_string, id, folder_name, toggle_partial = True ):
    for season in range(year_start, year_end+1):
        #print(season)
        # navigate to bballref
        url = page_string.replace("*SEASON*", str(season))
        browser.get(url)

        # setup id strings
        partial_id = id + "_toggle_partial_table"
        csv_id = "csv_" + id

        # activate and grab data in CSV format
        if toggle_partial:
            browser.execute_script('document.getElementById("'+ partial_id +'").click();')

        # grab raw CSV
        raw_csv = browser.execute_script('''var x = document.getElementsByClassName("tooltip");
        x[3].click();
        var content = document.getElementById("'''+ csv_id +'''")
        // small hack to account for a few old MVP pages
        if (content==null) {
            var content = document.getElementById("csv_mvp")
        }
        return content.textContent
        ''')

        # clean csv string
        ## get rid of false headers
        if page_string == 'https://www.basketball-reference.com/awards/awards_*SEASON*.html' or page_string == 'https://www.basketball-reference.com/leagues/NBA_*SEASON*_standings.html':
            raw_csv = '\n'.join(raw_csv.split('\n')[2:])
        ## remove special characters from standings
        raw_csv=raw_csv.replace("\u2264","")
        raw_csv=raw_csv.replace("\u2265","")

        #print(raw_csv)
        # write to CSV
        pathstr = folder_name + "/" + str(season) + ".csv"
        f = open(pathstr, "w")
        f.write(raw_csv)


### DEPRECATED, BUILT INTO scrapeBbalRef
## GRAB SEASON PLAYER STAT PER GAME
# for season in range(1976, 1977):
#     # navigate to bballref
#     url = 'https://www.basketball-reference.com/leagues/NBA_' + str(season) + '_per_game.html'
#     browser.get(url)
#     # activate and grab data in CSV format
#     raw_csv = browser.execute_script('''document.getElementById("per_game_stats_toggle_partial_table").click();
#     var x = document.getElementsByClassName("tooltip");
#     x[3].click();
#     var content = document.getElementById("csv_per_game_stats")
#     return content.textContent
#     ''')
#
#     # write to CSV
#     pathstr = "season-stats-pergame/" + str(season) + ".csv"
#     f = open(pathstr, "w")
#     f.write(raw_csv)
# ## GRAB SEASON PLAYER STAT TOTALS
# for season in range(1976, 2018):
#     # navigate to bballref
#     url = 'https://www.basketball-reference.com/leagues/NBA_' + str(season) + '_totals.html'
#     browser.get(url)
#     # activate and grab data in CSV format
#     raw_csv = browser.execute_script('''document.getElementById("totals_stats_toggle_partial_table").click();
#     var x = document.getElementsByClassName("tooltip");
#     x[3].click();
#     var content = document.getElementById("csv_totals_stats")
#     return content.textContent
#     ''')
#
#     # write to CSV
#     pathstr = "season-stats-totals/" + str(season) + ".csv"
#     f = open(pathstr, "w")
#     f.write(raw_csv)
# ## GRAB SEASON MVP STATS
# for season in range(1976, 1977):
#     # navigate to bballref
#     url = 'https://www.basketball-reference.com/awards/awards_' + str(season) + '.html'
#     browser.get(url)
#     # activate and grab data in CSV format
#     raw_csv = browser.execute_script('''var x = document.getElementsByClassName("tooltip");
#     x[3].click();
#     var content = document.getElementById("csv_mvp")
#     if (content==null) {
#         var content = document.getElementById("csv_nba_mvp")
#     }
#     return content.textContent
#     ''')
#
#     # write to CSV
#     raw_csv = raw_csv[133:]
#     pathstr = "award-stats/" + str(season) + ".csv"
#     f = open(pathstr, "w")
#     f.write(raw_csv)
