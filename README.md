[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Heatmaps of Selected Variables** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : Analyzing Crime Statistics in Toronto for 2016


Description: Using statistical methods to analyse crime in Toronto

Keywords: plot, vizualization, heatmap, regression, kmeans, poisson regression, spatial regression
          crime rates

Author: Gabriel Blumenstock, Felix Degenhardt, Haseeb Warsi

The purpose of this project was to gain a deeper understanding of the crime in different neighbourhoods in Toronto.
An preliminary analysis was performed by gathering census data, cleaning and formatting it, and then aggregating it 
into one dataset with the crime data. 

With a prepared dataset, key variables were plotted on a heat map to show differences 
and similarities between neighbourhoods. After plotting variables, several types of regressions were performed, in
order to find the relationship between neighbourhood characteristics and crime statistics.

Using these files:

The first step to using these files would be to open the project and download the data.zip file. After extracting them and
placing them in the approriate folder, you can begin using R. The main file to be used in ths project is SPL.R. SPL.R sources all 
the other scripts. This was done in an effort to keep script lengths manageable and allow us the flexibility 
to work on multiple scripts the same time. Simply run SPL.R and all other scripts should be sourced correctly.

It is possible to use much of this code for another city, provided the datasets come in a similar format. In the future that is something we may consider.
