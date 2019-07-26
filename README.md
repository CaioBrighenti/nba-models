# NBA Models

The goal of this project is to create a series of statistical models regarding NBA statistics, with the ultimate goal of implementing a website predicting odds for each major award and all star selections. This includes developing a comprehensive scraper to efficiently pull data from [Basketball Reference](https://www.basketball-reference.com/), respecting the website's Use of Data policies. The scraper is built in Python, and the data analysis is done in R.

This project also aims to eventually combine game statistics with social media data. There is no doubt all star voting and awards selection is heavily influenced by social media trends and media narratives, and I suspect accounting for this would improve the model. The first step for this will be including Twitter data, pending a developer account application.

## Towards Data Science

An article on this project has been published by [Towards Data Science](https://towardsdatascience.com/). This article can be found [here](https://towardsdatascience.com/using-data-science-to-predict-the-next-nba-mvp-30526e0443da).

## Getting Started

At this stage, usage of the project is fairly self explanatory. Simply use scraper.py for the scraping functionality, or loadData.R and MVPModels.R. Prerequisites for each of these, and libraries used are listed below or within the respective files.

## Currently Implemented

* Scraper for MVP data, season player total statistics, season player per game statistics, season player advanced statistics, and team standings
* Models predicting MVP outcome using MVP data and total and advanced player statistics
* Leave-one-out cross validation system to quickly evaluate new models

## Planed Features

**Scraper**
* ~~Add scraping for advanced statistics~~
* Add scraping for other awards

**MVP Models**
* ~~Model using per game statistics~~
* Accounting for shortened seasons
* Accounting for reverse recency bias
* Adding feature to analyze player improvement relative to past season
* ~~Add advanced stats to models~~
* Account for post All-Star break numbers

**Long-term**
* Create models for other awards
* Add social media data
* Merge data into single dataset and a build a system to query that dataset
* Deploy models into live-updating website

### Prerequisites

To use this code, you'll need a Python installation, along with Selenium with the appropriate drivers for the web crawler. You'll also need a R distribution installed -- I recommend RStudio. All of this is free to use, with links provided below.

* [Python](https://www.python.org/downloads/)
* [Selenium](https://selenium-python.readthedocs.io/)
* [RStudio](https://www.rstudio.com/products/rstudio/download/)

## Deployment

The goal for this project is to ultimately deploy it live onto a website, but the project is far from that point. Once the project is deployed, this README will be updated to include deployment instructions.

## Built With

* [Atom](https://flight-manual.atom.io/getting-started/sections/installing-atom/) - Text editor used
* [Hydrogen](https://atom.io/packages/hydrogen) - Interactive coding environment for Atom
* [RStudio](https://www.rstudio.com/) - IDE for R
* [Selenium](https://selenium-python.readthedocs.io/) - Framework for web crawling

## Contributing

Any contributions are welcome, as long as the code is functional and at least some effort is made to document it.

## Authors

* **Caio Brighenti** - *Developer* - [CaioBrighenti](https://github.com/CaioBrighenti)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* [Stack Overflow](https://stackoverflow.com/) - For obvious reasons
* [Harvard Sports Analytics](http://harvardsportsanalysis.org/2018/06/nba-mvp-predictions/) - For inspiration and a baseline comparison
