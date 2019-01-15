import selenium
from selenium import webdriver
import os

# start browser crawler
browser = webdriver.Firefox()

for season in range(1976, 1980):
    # navigate to bballref
    url = 'https://www.basketball-reference.com/leagues/NBA_' + str(season) + '_totals.html'
    browser.get(url)
    # activate and grab data in CSV format
    raw_csv = browser.execute_script('''var x = document.getElementsByClassName("tooltip");
    x[3].click();
    var content = document.getElementById("csv_totals_stats")
    return content.textContent
    ''')

    # write to CSV
    pathstr = "season-stats/" + str(season) + ".csv"
    os.getcwd()
    f = open(pathstr, "w")
    f.write(raw_csv)
