:: Clean Compiled Units
del /s /q *.dcu
del /s /q *.exe
del /s /q *.tmp
del /s /q *.rsm
del /s /q *.dproj.local
del /s /q *.identcache

:: Clean Embarcadero Directories
for /f "usebackq" %%a in (`"dir /ad/b/s __history"`) do rmdir /q /s "%%a"
for /f "usebackq" %%a in (`"dir /ad/b/s __recovery"`) do rmdir /q /s "%%a"