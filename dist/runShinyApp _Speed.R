message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))

#--------------------------------------------------------------------
# Edit this to change the browser application/location

chrome.sys = "C:/Program Files/Google/Chrome/Application/chrome.exe"
#--------------------------------------------------------------------

launch.browser = function(appUrl, browser.path=chrome.sys) {
    message('Browser path: ', browser.path)
    shell(sprintf('"%s" --app=%s', browser.path, appUrl))
}

shiny::runApp('./shiny_speed/', launch.browser=launch.browser)
