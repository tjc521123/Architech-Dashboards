#!/bin/bash

SET ROPTS=--no-save --no-environ --no-init-file --no-restore --no-Rconsole
R-Portable/App/R-Portable/bin/Rscript.exe %ROPTS% runShinyApp_Speed.R 1> ShinyApp.log 2>&1