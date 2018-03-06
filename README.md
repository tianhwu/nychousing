# nychousing

Plots housing data for NYC for 2003 to 2017 in an interactive R-shiny app.

You can find the output here:
https://tianhwu.shinyapps.io/nychousing/


Primary dataset is the NYC Housing Sales Data Found in this Link: http://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page


The data in all the separate excel spreadsheets for 2003-2017 was merged via VBA scripting in Excel and further cleaned & de-duped in R. All non-residential rows and large tenant housing units were dropped (based on the top residential building codes by quantity of transactions) The sales_price is inflation adjusted in terms of 2017 dollars.

Neighborhoods were mapped, to TIGRIS nyc neighborhood data manually (control data to be added here....).

Due to size constraint, me being too poor to afford to use geolocation apis of addresses, and not being able to correctly map neighborhoods I rarely go to, I dropped Staten Island and the Bronx.
