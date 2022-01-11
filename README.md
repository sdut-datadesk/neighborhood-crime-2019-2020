# Crime analysis for city of San Diego block groups 2019-2020
By: [Greg Moran](https://www.sandiegouniontribune.com/sdut-greg-moran-staff.html), [David Hernandez](https://www.sandiegouniontribune.com/sdut-david-hernandez-staff.html) and [Lauryn Schroeder](https://www.sandiegouniontribune.com/sdut-lauryn-schroeder-staff.html)

This repository contains data and code for the analysis [reported and published](https://www.sandiegouniontribune.com/news/public-safety/story/2021-10-31/crime-counts-violent-crime-increased-in-san-diego-in-pandemic-year-but-not-as-much-as-national-trend) by *The San Diego Union-Tribune* on Oct. 31, 2021.

### About

The San Diego Union-Tribune set out to analyze and review crime on a neighborhood and street-level basis, and potentially reveal how violent incidents can impact the lives and experiences of residents throughout the city.

Working with the San Diego Associations of Governments, or SANDAG, which collects crime data from local police departments and the San Diego County Sheriff’s Department, the Union-Tribune acquired records of all crime activity that occurred in the county on a census block group level.

The Automated Regional Justice Information System (ARJIS), which serves as a repository for criminal justice data submitted by local police agencies, assisted SANDAG with the requests.

Data files have been used to [report and publish](https://www.sandiegouniontribune.com/story/2019-03-28/crime-counts-as-crime-falls-to-record-lows-some-neighborhoods-are-left-behind) an analysis of crime from 2013 through 2017.

A second story analyzed [crime trends from 2014 through 2018](https://www.sandiegouniontribune.com/news/public-safety/story/2019-06-05/more-city-neighborhoods-see-increase-in-violent-crimes-new-data-analysis-shows).

The entire collection consists of eight years’ worth of data showing information about each crime incident, such as the date and time the crime occurred, when it was reported, where it took place (by block address, city and ZIP code), the reporting agency and the description of the crime, as defined by the FBI’s Uniform Crime Reporting Program.

All information is provided by local agencies.

### Methodology / Notes

Unlike most crime data collections and statistics, which tend to be frozen in time and represent an as-of-this-day count, the Union-Tribune obtained data reflecting both the original report and any subsequent updates or changes.

For example, an aggravated assault could be re-classified as a homicide if the victim dies. The data provided to the Union-Tribune reflects such changes.

These changes are reflected in a separate field, known as the “number actually reported,” which has either a positive number — indicating a crime occurred — or a negative number, indicating the crime changed or was proven unfounded.

The Union-Tribune used this field to calculate all crime totals in the analysis.

The Union-Tribune filtered the database to include only violent crimes — robberies, strong arm robberies, rapes, attempted rapes, murders and aggravated assaults — that listed San Diego police as the responding agency. Crimes that were reported between 2019 and 2020 but occurred outside of that time frame were not included in the analysis. Crimes that were reported to local agencies but occurred outside the city of San Diego were also removed.

The Union-Tribune also chose to filter the database to a selection of non-violent, lower-level crimes, to compare the pre-pandemic year of 2019 to the largely pandemic year of 2020.

These crimes are larceny, residential burglary, fraud, commercial burglaries, malicious mischief, and some domestic crimes.

Not all block groups fit perfectly inside cities and census-designated places. For example, some are divided by the boundary lines of San Diego and surrounding cities such as Chula Vista, Lemon Grove and Poway.

The Union-Tribune used the Missouri Census Data Center’s geographic correspondence engine, a data tool, to determine which block groups fall within or on a boundary of the city of San Diego. The Union-Tribune's analysis includes all crimes that occurred in a block group that falls inside or partially within the city's jurisdiction line.

All crime records are submitted to ARJIS by member agencies and therefore can contain some level of error, such as missing or incomplete information. When address information is missing or incorrect, sometimes agencies will mark the crime location as a detention facility, courthouse, interstate or border patrol checkpoint. Less than 1 percent of total rows in the database were removed from the analysis due to missing or incomplete address information.

### The SDUT repository contains the following:

- `Final_PRA_Request_2020_Crime_data.csv` - Crime data provided by SANDAG and ARJIS for 2020.
- `SANDAG-crime-2019.csv` - Crime data provided by SANDAG and ARJIS for 2019.
- `pop2019.csv` - Census ACS 5-year population estimates for 2019 by census block groups. Includes all block groups in San Diego County.
- `sdut-crime-counts-analysis-2019-2020.R` - Import and analysis R script documenting findings published by the Union-Tribune.

### Sourcing
Please link and source [*The San Diego Union-Tribune*](https://www.sandiegouniontribune.com/) when referencing any analysis or findings in published work.

### Questions / Feedback

Email Lauryn Schroeder at [lauryn.schroeder@sduniontribune.com](mailto:lauryn.schroeder@sduniontribune.com)
