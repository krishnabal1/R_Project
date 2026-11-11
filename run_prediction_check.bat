@echo off
cd /d "%~dp0"
set "RSCRIPT="
for /d %%R in ("%ProgramFiles%\R\R-*") do if exist "%%~fR\bin\Rscript.exe" set "RSCRIPT=%%~fR\bin\Rscript.exe"
if not defined RSCRIPT (
  echo Rscript.exe was not found under "%ProgramFiles%\R".
  echo Install R or add Rscript.exe to PATH, then try again.
  pause
  exit /b 1
)
"%RSCRIPT%" predict_new_question.R
if errorlevel 1 pause
