import os,sys, subprocess


stata_program_fpath="c:\\Program Files (x86)\\Stata14\\Stata-64.exe"
ngr_data = {'file1':"c:/local_files/research/consumption/lsms/data/ngr_df2010.dta", 'file2':"c:/local_files/research/consumption/lsms/data/ngr_df2012.dta", 'file3':"c:/local_files/research/consumption/lsms/data/ngr_df2015.dta" }
settings = {'NGR_direct': {'operation':'direct','data': ngr_data} , 
            'NGR_hilo_A': {'operation':'hilo','data': ngr_data , 'split_field':'lnA0'} }

temp_file = 'c:/temp/resnu.tex'
if not os.path.exists(stata_program_fpath):
    raise RuntimeError("Invalid Stata exec path")

if os.path.exists(temp_file):
    print("File exists")
    os.remove(temp_file)

if len(sys.argv)< 4:
    print("Must have all arguments")
    sys.exit(1)

dofpath = sys.argv[1]
depvar  = sys.argv[2]
settings_name = sys.argv[3]

if settings_name not in settings:
    raise RuntimeError("Unknown settings name : %s " % settings_name )

if not os.path.exists(dofpath):
    raise RuntimeError("Invalid do-filepath")

if settings[settings_name]['operation'] == 'direct':
    selected_settings_data= settings[settings_name]['data']
    
    retcode=subprocess.check_call([stata_program_fpath,"/e","do",dofpath,depvar,
                                   selected_settings_data['file1'],
                                   selected_settings_data['file2'],
                                   selected_settings_data['file3']])
    sys.exit(0)
    
if settings[settings_name]['operation'] == 'hilo':
    source_data= settings[settings_name]['data']
    lo_file = "c:/temp/lo.dta"
    hi_file = "c:/temp/hi.dta"
    try :        
        retcode=subprocess.check_call(["Rscript", "split_data.R",source_data['file1'],
                                       settings[settings_name]['split_field'],lo_file,hi_file])
    except Exception as e:
        raise e
        
    if retcode>0:
        raise RuntimeError("R script failed")
    sys.exit(0)

raise RuntimeError("Unknown operation")
