# Architech-Dashboards

## Initial Set-Up

First, download the zip file of the `Architech-Dashboards` repository.
Extract the zip to the desired location. The main folder of interest is the `dist` folder.

Throughout this process, be mindful of whether you have hidden file extensions. This is the default setting on most computers, so if you do not see `.exe` or `.bat`, it is because the extension is hidden.

You must download and install `R Portable` from:
	https://sourceforge.net/projects/rportable/

The location of the installation should be the `dist` folder. 

The `dist` folder should now contain:
* R-Portable
* Monthly Testing
* Speed

After installing, launch R Portable by running the `R-Portable.exe` file in the `R-Portable` folder. Once it is open, install `shiny` and `pacman`.
* Run the following commands in the `R-Portable` console to install the packages:
	* `install.packages('shiny')`
	* `install.packages('pacman')`
 * This may take a few minutes.

## Running the Application

Double-click on the correct `run` file depending on your operating systme. This should open the file. 

### Dependencies

The application needs Google Chrome installed in its default location to operate. 
If you want to change the browser application/location, you must edit the `runShinyApp.R`
file with:
	`chrome.sys = "new/application/location.exe"`

The new application location **MUST** be double-quoted "".

## `Speed_Template.xlsx`

The included `Speed_Template.xlsx` is a demo spreadsheet that has some sample data to showcase the features of the dashboard. Additionally, it is in the correct format for the dashboard to use, so when you go to log your own speed data, you can use the template.
