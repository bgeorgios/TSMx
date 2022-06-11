# TSMx
A visualization tool for plotting multiscale temporal trends. 

TSMx is an R script that was developed to facilitate multi-temporal-scale visualizations of time-series data. The script requires only a two-column CSV of years and values to plot the slope of the linear regression line for all possible year combinations from the supplied temporal range. The outputs include a time-series matrix showing slope direction based on the linear regression, slope values plotted with colors indicating magnitude, and results of a Mann-Kendall test. The start year is indicated on the y-axis and the end year is indicated on the x-axis. In the example below, the cell in the top-right corner is the direction of the slope for the temporal range 2001–2019. The red line corresponds with the temporal range 2010–2019 and an arrow is drawn from the cell that represents that range. One cell is highlighted with a black border to demonstrate how to read the chart—that cell represents the slope for the temporal range 2004–2014.

*TSMx for R was developed by Georgios Boumis; TSMx was originally conceptualized and created by Brad G. Peter in Microsoft Excel.*

This tool accompanies a publication in preparation by Peter et al. (forthcoming): Peter, B. G., Messina, J. P., Breeze, V., Fung, C., Kapoor, A., and Fan, P. (in prepration) Modifiable spatiotemporal unit problems in remote sensing of agriculture: Evidence from rice production in Vietnam. Journal TBD. 

## Need Help?
Need help using RRIr scripts? Found an issue? Have a feature request? Check out my
[personal blog](http://www.gboumis.com) and contact me via my LinkedIn profile found at the header.
