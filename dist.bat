@echo off
setlocal enabledelayedexpansion
if exist "Dist" rd /s /q "Dist"

set "BDS_REG_KEY=HKLM\SOFTWARE\WOW6432Node\Embarcadero\BDS"
set "LATEST_VERSION=0.0"
set "DELPHI_PATH="

set "LIB_DIR=.\Libraries"
set "OPENSSL_DIR=%LIB_DIR%\OpenSSL"
set "ZIP_FILE=%OPENSSL_DIR%\openssl-1.1.1w.zip"
set "TEMP_EXTRACT=%OPENSSL_DIR%\temp_extracted"
set "DEST_X32=%LIB_DIR%\OpenSSL\x32"
set "DEST_X64=%LIB_DIR%\OpenSSL\x64"
set "CRYPTO_X32=libcrypto-1_1.dll"
set "SSL_X32=libssl-1_1.dll"
set "CRYPTO_X64=libcrypto-1_1-x64.dll"
set "SSL_X64=libssl-1_1-x64.dll"
set "SRC_X32=%TEMP_EXTRACT%\openssl-1.1\x86\bin"
set "SRC_X64=%TEMP_EXTRACT%\openssl-1.1\x64\bin"
set "PACKAGE_VIRTUALTREE=VirtualTreeview_by_JamSoftware-13-8.3"

:: ------------------------------------------------------------------------------------------------------------
:: 1. Retrieve the Delphi Compiler installation path along with the necessary information and scripts to 
::	  compile using its command-line tool.
:: ------------------------------------------------------------------------------------------------------------
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
    echo [x] Delphi installation not found in Registry.
    exit /b 1
)

set "RSVAR_FILE=%DELPHI_BIN_DIRECTORY%\rsvars.bat"

if not exist "%RSVAR_FILE%" (
	echo [x] Delphi Resource String Variables File not found, ensure you are using a professional Delphi version
	exit /b 2
)

:: ------------------------------------------------------------------------------------------------------------
:: 2. Clean up compiled units, binaries, and unnecessary post-compilation artifacts.
:: ------------------------------------------------------------------------------------------------------------
call clean.bat2^>nul

:: ------------------------------------------------------------------------------------------------------------
:: 3. Call `rsvar` to initialize required runtime variables and ensure proper compiler behavior. 
:: ------------------------------------------------------------------------------------------------------------
call "%RSVAR_FILE%"

:: ------------------------------------------------------------------------------------------------------------
:: 4. Extract and prepare the required OpenSSL binaries for the Optix OpenSSL version.
:: ------------------------------------------------------------------------------------------------------------
if not exist "%ZIP_FILE%" (
    echo [x] The file %ZIP_FILE% does not exist.
    exit /b 3
)

echo [i] Extracting %ZIP_FILE%...
if exist "%TEMP_EXTRACT%" rmdir /s /q "%TEMP_EXTRACT%"
powershell -NoProfile -Command "Expand-Archive -Path '%ZIP_FILE%' -DestinationPath '%TEMP_EXTRACT%' -Force"

for %%F in (
    "%SRC_X64%\%CRYPTO_X64%"
    "%SRC_X64%\%SSL_X64%"
    "%SRC_X32%\%CRYPTO_X32%"
    "%SRC_X32%\%SSL_X32%"
) do (
    if not exist "%%~F" (
        echo [x] Required file %%~nxF was not found after extraction.
        rmdir /s /q "%TEMP_EXTRACT%" 2>nul
        exit /b 4
    )
)

if not exist "%DEST_X64%" mkdir "%DEST_X64%"
if not exist "%DEST_X32%" mkdir "%DEST_X32%"

echo [i] Copying x32 binaries...
copy /Y "%SRC_X32%\%CRYPTO_X32%" "%DEST_X32%\"
copy /Y "%SRC_X32%\%SSL_X32%" "%DEST_X32%\"

echo [i] Copying x64 binaries...
copy /Y "%SRC_X64%\%CRYPTO_X64%" "%DEST_X64%\"
copy /Y "%SRC_X64%\%SSL_X64%" "%DEST_X64%\"

echo [i] Cleaning up temporary extraction folder...
rmdir /s /q "%TEMP_EXTRACT%"

echo [i] OpenSSL binaries successfully extracted!

REM Define Required Libraries
set "BASE_DIR=%~dp0"

:: ------------------------------------------------------------------------------------------------------------
:: 5. Install Required Package(s) using GetIt
:: ------------------------------------------------------------------------------------------------------------

echo [i] Checking for %PACKAGE_VIRTUALTREE%...
GetItCmd.exe -l="" -f=installed | findstr /i /c:"%PACKAGE_VIRTUALTREE%" >nul

REM (!) Manual installation via the GetIt Package Manager in the Delphi IDE is required; a bug currently affects
REM the command-line version.

if %errorlevel% neq 0 (
	echo [x] %PACKAGE_VIRTUALTREE% must be manually installed via the Delphi IDE GetIt Package Manager before running the disk script.
	exit /b 5
)

:: if %errorlevel% equ 0 (
::     echo [+] %PACKAGE_VIRTUALTREE% is already installed.
::     goto :SkipGetItVirtualTree
:: )

:: if %errorlevel% equ 0 (
::     echo [+] %PACKAGE_VIRTUALTREE% is already installed.
::     goto :SkipGetItVirtualTree
:: )

:: echo [i] Package not found. Installing %PACKAGE_VIRTUALTREE%...
:: GetItCmd.exe -i="%PACKAGE_VIRTUALTREE%" -ae

:: if %errorlevel% neq 0 (
::     echo [x] Installation failed.
::     pause
::     exit /b 5
:: )

:: echo [+] Installation complete!

:: :SkipGetItVirtualTree

:: ------------------------------------------------------------------------------------------------------------
:: 6. Compile the project applications and organize their respective structures in the Dist directory.
:: ------------------------------------------------------------------------------------------------------------
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
                if %errorlevel% equ 0 (
                    REM Copy Optix NoSSL
                    copy /Y "%%I\bins\NoSSL\%%X\Release\*.exe" "Dist\%%Y\%%K\NoSSL\"
                )
                
                msbuild "%%I\%%J_OpenSSL.dproj" /t:Build /p:Config=Release /p:Platform=%%X"
                if %errorlevel% equ 0 (
                    REM Copy Optix OpenSSL
                    copy /Y "%%I\bins\OpenSSL\%%X\Release\*.exe" "Dist\%%Y\%%K\OpenSSL\"
                    copy /Y "Libraries\OpenSSL\%%Y\*.dll" "Dist\%%Y\%%K\OpenSSL\"
                ) 
            )
        )
    )
)