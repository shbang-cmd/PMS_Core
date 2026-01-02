@echo off

REM =============================================
REM PMS Gemini AI Email Runner
REM Logs all stdout/stderr with timestamps
REM =============================================

set REXE="C:\PROGRA~1\R\R-45~1.2\bin\Rscript.exe"
set RSCRIPT="C:\PMS_Core\gemini_ai_email.R"

set LOGDIR=C:\PMS_Core\log
if not exist %LOGDIR% mkdir %LOGDIR%

set YYYY=%DATE:~0,4%
set MM=%DATE:~5,2%
set DD=%DATE:~8,2%
set HH=%TIME:~0,2%
set MI=%TIME:~3,2%
set SS=%TIME:~6,2%
set HH=%HH: =0%

set LOGFILE=%LOGDIR%\gemini_ai_email_%YYYY%%MM%%DD%_%HH%%MI%%SS%.log

echo ============================================= >> %LOGFILE%
echo START : %DATE% %TIME% >> %LOGFILE%
echo SCRIPT: %RSCRIPT% >> %LOGFILE%
echo ============================================= >> %LOGFILE%

%REXE% %RSCRIPT% >> %LOGFILE% 2>&1

set EXITCODE=%ERRORLEVEL%

echo END   : %DATE% %TIME% >> %LOGFILE%
echo EXIT  : %EXITCODE% >> %LOGFILE%
