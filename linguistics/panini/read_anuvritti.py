import pandas as pd

"""
There can be no dangling inserts. The only new nodes are at roots. A new L3 goes to L2 (not anything before L2). Therefore, Ln is inserted only in L(n-1).
Further insertion of Ln is recursive so the insert only happens after L(n+1)...L(N) have been parsed.
"""
def find_first_non_na(dat):
	if "L1" in dat and not pd.isnull(dat.L1):
		return 1
	if "L2" in dat and not pd.isnull(dat.L2):
		return 2
	if "L3" in dat and not pd.isnull(dat.L3):
		return 3
	if "L4" in dat and not pd.isnull(dat.L4):
		return 4
	if "L5" in dat and not pd.isnull(dat.L5):
		return 5
	if "L6" in dat and not pd.isnull(dat.L6):
		return 6
	if "L7" in dat and not pd.isnull(dat.L7):
		return 7
	if "L8" in dat and not pd.isnull(dat.L8):
		return 8
	if "L9" in dat and not pd.isnull(dat.L9):
		return 9
	if "L10" in dat and not pd.isnull(dat.L10):
		return 10
	return None


class Struct:
	def __init__(self):
		self.struct =[[]]
		
		

	def add(self,level,data):
		if not isinstance(level,int):
			raise ValueError("Not in: %s" % level)
		self.struct[-1][level-1].append(data)


		

b = pd.read_excel('c:/temp/test.xlsx')
current_level = 1
struct = []
for i in range(0,b.shape[0]):
	row= (b.loc[i])
	non_na_level = find_first_non_na(row[(current_level-1):])
	while non_na_level:
		struct.append(non_na_level)
		current_level = current_level+1
		non_na_level = find_first_non_na(row[(current_level-1):])



