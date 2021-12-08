import re,os,sys, subprocess, time
import shutil


def remove_if_exists(x):
    if os.path.exists(x):
        os.remove(x)
                
    
stata_program_fpath="c:\\Program Files (x86)\\Stata14\\Stata-64.exe"
ngr_data = {'file1':"c:/local_files/research/consumption/lsms/data/ngr_df2010.dta", 'file2':"c:/local_files/research/consumption/lsms/data/ngr_df2012.dta", 'file3':"c:/local_files/research/consumption/lsms/data/ngr_df2015.dta" }
tn_data = {'file1':"c:/local_files/research/consumption/lsms/data/tn_df2010.dta", 'file2':"c:/local_files/research/consumption/lsms/data/tn_df2012.dta", 'file3':"c:/local_files/research/consumption/lsms/data/tn_df2014.dta" }
settings = {'NGR_direct': {'operation':'direct','data': ngr_data} , 
            'NGR_hilo_A': {'operation':'hilo','data': ngr_data , 'split_field':'lnA0'},
            'NGR_hilo_x': {'operation':'hilo','data': ngr_data , 'split_field':'logx'},
            'TNZ_direct': {'operation':'direct','data': tn_data} , 
            'TNZ_hilo_A': {'operation':'hilo','data': tn_data , 'split_field':'lnA0'} ,
            'TNZ_hilo_x': {'operation':'hilo','data': tn_data , 'split_field':'logx'} }


if not os.path.exists(stata_program_fpath):
    raise RuntimeError("Invalid Stata exec path")

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
    
    temp_file = 'c:/temp/resnu.tex'
    remove_if_exists(temp_file)
    time_before_call = time.time()
            
    retcode=subprocess.check_call([stata_program_fpath,"/e","do",dofpath,depvar,
                                   selected_settings_data['file1'],
                                   selected_settings_data['file2'],
                                   selected_settings_data['file3']])
    
    if (os.path.getmtime(temp_file) <time_before_call):
        raise RuntimeError("File Pointed to is before the call")

    sys.exit(0)
    
if settings[settings_name]['operation'] == 'hilo':
    source_data= settings[settings_name]['data']
    lo_file_path_prefix = "c:/temp/lo_"
    hi_file_path_prefix = "c:/temp/hi_"
    data_files = {}
    for file_field in ('file1','file2','file3'):
        lo_file=lo_file_path_prefix +file_field+".dta"
        hi_file=hi_file_path_prefix +file_field+".dta"
        remove_if_exists(lo_file)
        remove_if_exists(hi_file)
        
        try :
            
            retcode=subprocess.check_call(["Rscript", "split_data.R",source_data[file_field],
                                           settings[settings_name]['split_field'],lo_file,hi_file])
            
        except Exception as e:
            raise e
        
        if retcode>0:
            raise RuntimeError("R script failed")
        else:
            data_files[file_field]={'lo_file':lo_file,'hi_file':hi_file}
    print(type(data_files['file1']))       
    print(data_files)
    file_types = ['lo_file','hi_file']
    for ftype in file_types :
        temp_file = 'c:/temp/resnu.tex'
        remove_if_exists(temp_file)
        time_before_call = time.time()
        print("ftype="+str(ftype))
        
        retcode=subprocess.check_call([stata_program_fpath,"/e","do",dofpath,depvar,
                                   data_files['file1'][ftype],
                                   data_files['file2'][ftype],
                                   data_files['file3'][ftype]])
        
        if (os.path.getmtime(temp_file) <time_before_call):
            raise RuntimeError("File Pointed to is before the call")
        shutil.copyfile(temp_file,re.sub("\\.tex$","_"+ftype+".tex",temp_file))
        

    sys.exit(0)

raise RuntimeError("Unknown operation")
