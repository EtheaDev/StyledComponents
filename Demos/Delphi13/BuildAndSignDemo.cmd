@rem ---- Explorer pause: when started by double click the window stays open at the end ----
@if defined EXPLORER_PAUSE_CHILD goto :explorer_pause_skip
@if not "%~1"=="" goto :explorer_pause_skip
@echo %cmdcmdline% | "%SystemRoot%\System32\find.exe" /i "%~nx0" > NUL
@if errorlevel 1 goto :explorer_pause_skip
@echo %cmdcmdline% | "%SystemRoot%\System32\find.exe" /i "call " > NUL
@if not errorlevel 1 goto :explorer_pause_skip
@set EXPLORER_PAUSE_CHILD=1
@"%ComSpec%" /c ""%~f0""
@set EXPLORER_PAUSE_RC=%errorlevel%
@echo.
@pause
@exit /b %EXPLORER_PAUSE_RC%
:explorer_pause_skip
@echo *****************************************
@echo BUILD StyledComponentsDemo with Delphi 13
@echo *****************************************
call "C:\BDS\Studio\37.0\bin\rsvars.bat"
msbuild.exe ".\StyledComponentsDemo.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release
msbuild.exe ".\StyledComponentsDemo.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release

@echo ****************************************
@echo SIGN StyledComponentsDemo with Delphi 13
@echo ****************************************
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\StyledComponents\Demos\Bin\Win64\StyledComponentsDemo.exe
