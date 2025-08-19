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
