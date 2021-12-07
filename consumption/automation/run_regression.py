import os,sys, subprocess


stata_program_fpath="c:\\Program Files (x86)\\Stata14\\Stata-64.exe"
temp_file = 'c:/temp/resnu.tex'
if not os.path.exists(stata_program_fpath):
    raise RuntimeError("Invalid Stata exec path")

if os.path.exists(temp_file):
    print("File exists")
    os.remove(temp_file)

if len(sys.argv)< 3:
    print("Must have arguments")
    sys.exit(1)

dofpath = sys.argv[1]
depvar  = sys.argv[2]

if not os.path.exists(dofpath):
    raise RuntimeError("Invalid do-filepath")

retcode=subprocess.check_call(["Rscript", "split_data.R",dofpath])

if False:
    retcode=subprocess.check_call([stata_program_fpath,"/e","do",dofpath,depvar])
    print(retcode)
"""


c:\Program Files (x86)\Stata14\Stata-64.exe /e do %dofpath% %depvar%

rem DONE
"""
