@echo off

REM ── 일요일 체크 (0=일요일, 1=월요일, ..., 6=토요일)
for /f %%a in ('powershell -command "(Get-Date).DayOfWeek.value__"') do set DOW=%%a

if "%DOW%"=="0" (
    echo [PMS] Today is Sunday. Skip batch execution.
    exit /b 0
)

REM ── 배치 실행 신호 (일시적 환경변수)
set PMS_BATCH_RUN=1

REM ── PMS 실행 + 로그 저장
"C:\PROGRA~1\R\R-45~1.2\bin\Rscript.exe" "C:\PMS_Core\pms_main.R" > "C:\PMS_Core\pms_main.log" 2>&1

REM ── 배치 종료 (작업스케줄러 Running 방지)
exit /b 0

