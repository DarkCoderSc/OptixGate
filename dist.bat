@echo off
setlocal enabledelayedexpansion
if exist "Dist" rd /s /q "Dist"

REM Prepare Delphi Env
set "BDS_REG_KEY=HKLM\SOFTWARE\WOW6432Node\Embarcadero\BDS"
set "LATEST_VERSION=0.0"
set "DELPHI_PATH="

for /f "tokens=6 delims=\" %%A in ('reg query "%BDS_REG_KEY%" /f * /k') do (
    set "CURRENT_VERSION=%%A"
    if !CURRENT_VERSION! GTR !LATEST_VERSION! (
        set "LATEST_VERSION=!CURRENT_VERSION!"
    )
)

for /f "tokens=2*" %%A in ('reg query "%BDS_REG_KEY%\%LATEST_VERSION%" /v RootDir 2^>nul') do (
    set "DELPHI_PATH=%%B"
)

if defined DELPHI_PATH (
    set "DELPHI_BIN_DIRECTORY=%DELPHI_PATH%bin"
) else (
    echo Delphi installation not found in Registry.
    exit /b 1
)

call clean.bat
call "%DELPHI_BIN_DIRECTORY%\rsvars.bat"

REM Define Required Libraries
set "BASE_DIR=%~dp0"

REM Format: "ProjectFolder | ProjectFile | OutputFolder"
for %%A in (
    "Client GUI|Client_GUI|Client_GUI"
    "Server|OptixGate|Server"
) do (
    for /F "tokens=1,2,3 delims=|" %%I in ("%%~A") do (
        REM %%I = Folder
        REM %%J = Project File
        REM %%K = Output Folder

        for %%B in (
            "Win32|x32"
            "Win64|x64"
        ) do (
            for /F "tokens=1,2 delims=|" %%X in ("%%~B") do (
                REM %%X = Platform
                REM %%Y = Arch Folder

                REM Create directories
                mkdir "Dist\%%Y\%%K\NoSSL\" 2>nul
                mkdir "Dist\%%Y\%%K\OpenSSL\" 2>nul

                REM Build Optix NoSSL & Optix OpenSSL
                msbuild "%%I\%%J.dproj" /t:Build /p:Config=Release /p:Platform=%%X"
                msbuild "%%I\%%J_OpenSSL.dproj" /t:Build /p:Config=Release /p:Platform=%%X"

                REM Copy Optix NoSSL
                copy /Y "%%I\bins\NoSSL\%%X\Release\*.exe" "Dist\%%Y\%%K\NoSSL\"

                REM Copy Optix OpenSSL
                copy /Y "%%I\bins\OpenSSL\%%X\Release\*.exe" "Dist\%%Y\%%K\OpenSSL\"
                copy /Y "Libraries\LibOpenSSL\%%X\*.dll" "Dist\%%Y\%%K\OpenSSL\"
            )
        )
    )
)

pause
