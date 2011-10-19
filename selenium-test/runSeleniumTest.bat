@ECHO OFF

IF %1.==. GOTO :SHOW_HELP

IF "%1"=="all" GOTO :RUN_ALL

GOTO :RUN_ONE

:SHOW_HELP
echo usage selenium-test\runSeleniumTest.bat all or spec-name.rb
GOTO End1

:RUN_ALL
for /F %%f in ('dir /b selenium-test\src\specs') do spec --colour --format specdoc selenium-test\src\specs\%%f
GOTO End1

:RUN_ONE
spec --colour --format specdoc selenium-test\src\specs\%1
GOTO End1

:End1
