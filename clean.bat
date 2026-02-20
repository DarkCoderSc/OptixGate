REM Clean Compiled Units
del /s /q *.dcu *.exe *.tmp *.rsm *.dproj.local *.identcache 2>nul

REM Clean Embarcadero Directories
for /f "delims=" %%a in ('dir /ad /b /s __history 2^>nul') do rmdir /q /s "%%a" 2>nul
for /f "delims=" %%a in ('dir /ad /b /s __recovery 2^>nul') do rmdir /q /s "%%a" 2>nul
