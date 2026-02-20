@echo off
if exist "Dist" rd /s /q "Dist"

REM Prepare Delphi Env
call clean.bat
call "%ProgramFiles(x86)%\Embarcadero\Studio\37.0\bin\rsvars.bat"

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
                msbuild "%%I\%%J.dproj" /t:Build /p:Config=Release /p:Platform=%%X
                msbuild "%%I\%%J_OpenSSL.dproj" /t:Build /p:Config=Release /p:Platform=%%X

                REM Copy Optix NoSSL
                copy /Y "%%I\bins\NoSSL\%%X\Release\*.exe" "Dist\%%Y\%%K\NoSSL\"

                REM Copy Optix OpenSSL
                copy /Y "%%I\bins\OpenSSL\%%X\Release\*.exe" "Dist\%%Y\%%K\OpenSSL\"
                copy /Y "ExtLibraries\LibOpenSSL\%%X\*.dll" "Dist\%%Y\%%K\OpenSSL\"
            )
        )
    )
)

pause
