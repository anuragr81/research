rem R CMD BATCH

del c:\temp\resnu.tex

set dofpath=%1
set depvar=%2

c:\"Program Files (x86)"\Stata14\Stata-64.exe /e do %dofpath% %depvar%

rem DONE