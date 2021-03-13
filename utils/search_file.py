import re,os
from functools import reduce

def search_path(path,pattern):
	files = [ f for f in os.listdir(path) if os.path.isfile(path + "/"+ f)]
	dirs = [ path+"/"+ f for f in os.listdir(path) if os.path.isdir(path + "/" + f)]
	matched = [ path + "/" + f for f in files if re.search(pattern,f) ]
	if dirs:
		dat = [search_path(d,pattern) for d in dirs]
		return reduce(lambda x,y: x + y, dat,[]) + matched
	else:
		return matched
    
    
if __name__ == "__main__":
    print(search_path("c:/local_files/research","search_file"))