import pydot

expenseTree= {
'intoxication' :['cigarettes','beer','brews','winespirits'],
'home_energy':['charcoal','electricity','gas','kerosene','matches'],
'transport':['petrol','public_transport',],
'communications':['cellphone_voucher','phone',],
'personal_products':['bar_soap','toothpaste','shampoo'],
'household_products':['clothes_soap','toilet_paper','bulbs','misc_cleaning','carpet','linen','mat','mosquito_net','mattress',],
'household_repair':['household_products_repair','house_repair','motor_repair','bicycle_repair','consumer_durables_repair',],
'appliances':['light_bulbs','sports_hobby','camera',],
'charity':['donation'],
'services':['services'],
'housing':['mortgage','council_rates','building_material','bamboo','grass'],
'legalfinance':['insurance','legal'],
'social_events':['marriage','bride_price','funeral'],
'food': {'beverages':['water','tea','coffee','miscdrinkpowder','canned_drink','readymade_tea','readymade_coffee',],
         'starch': ['rice_husked','rice_paddy','maize_green','maize_grain','maize_flour','millet_grain','millet_flour','wheat','bread','bunscakes','pasta','othercereal','pulses',] ,  # milling
         'vegstarch' : ['cassava_fresh','cassava_flour','sweet_potato','yam','potatoes','banana_green','othervegstarch'], 
         'sugars':['sugar','sweet','honey'], 
         'fat':['peanuts','coconut','cashew_almonds','nut_products','cooking_oil','butter_margarine',],
         'vegetables':['onion','greens','dried_canned_veg',] ,
         'fruits':['banana_ripe','citrus','mangoes','sugarcane'],
         'protein':['goat','beef','pork','chicken','wild_birds','wild_meat','fish_seafood','dried_canned_fish','packaged_fish','fresh_milk','milk_products','canned_mik'], 
         'condiments': ['spices','salt'] },
}

foodGroups = {
         'alcohol' :['beer','brews','winespirits'],
         'beverages':['tea','coffee','miscdrinkpowder','canned_drink','readymade_tea','readymade_coffee',],
         'starch': ['rice_husked','rice_paddy','maize_green','maize_grain','maize_flour','millet_grain','millet_flour','wheat','bread','bunscakes','pasta','othercereal','pulses',] ,
         'vegstarch' : ['cassava_fresh','cassava_flour','sweet_potato','yam','potatoes','banana_green','othervegstarch'], 
         'sugars':['sugar','sweet','honey'], 
         'fat':['peanuts','coconut','cashew_almonds','nut_products','milk_products'],
	     'oil':['cooking_oil','butter_margarine',],
         'vegetables':['onion','greens','dried_canned_veg',] ,
         'fruits':['banana_ripe','citrus','mangoes','sugarcane'],
         'meat':['goat','beef','pork','chicken','wild_birds','wild_meat','fish_seafood','dried_canned_fish','packaged_fish',],
     	 'milk': ['fresh_milk','canned_milk'], 
         'condiments': ['spices','salt'] ,
}

caloriesDict =  { 'rice_paddy': { 'calories': 360.*.8 },
		  'rice_husked': { 'calories': 360. },
		  'maize_green': { 'calories': 106. },
		  'maize_flour': { 'calories': 364. },
		  'maize_grain': { 'calories': 364. },
		  'millet_grain': { 'calories': 378. },
		  'millet_flour': { 'calories': 382. },
		  'wheat'       : {'calories': 327. },
		  'bread'       : {'calories': 267. },
		  'bunscakes'   : {'calories':324. },
		  'pasta'       : {'calories': 352. },
		  'othercereal' : {'calories': 300. },
		  'pulses'      : {'calories': 352. },
		  'cassava_fresh'       : {'calories': 160. },
		  'cassava_flour'       : {'calories': 159. },
		  'sweet_potato'        : {'calories': 86. },
		  'yam'                 : {'calories': 118. },
		  'potatoes'            : {'calories': 77. },
		  'banana_green'        : {'calories': 100. },
		  'othervegstarch'      : {'calories': 100. },
		  'sugar'               : {'calories': 387. },
		  'sweet'               : {'calories': 350. },
		  'honey'               : {'calories': 300. },
		  'peanuts'             : {'calories': 567. },
		  'coconut'             : {'calories': 350. },
		  'cashew_almonds'      : {'calories': 550. },
		  'nut_products'        : {'calories': 550. },
		  'cooking_oil'         : {'calories': 880. },
		  'butter_margarine'    : {'calories': 720. },
		  'onion'               : {'calories': 40. },
		  'greens'              : {'calories': 30. },
		  'dried_canned_veg'    : {'calories': 40. },
		  'banana_ripe'         : {'calories': 100. },
		  'citrus'              : {'calories': 45. },
		  'mangoes'             : {'calories': 60. },
		  'sugarcane'           : {'calories': 180. },
		  'goat'                : {'calories': 109. },
		  'beef'                : {'calories': 250. },
		  'pork'                : {'calories': 300. },
		  'chicken'             : {'calories': 160. },
		  'wild_birds'          : {'calories': 160. },
		  'fish_seafood'        : {'calories': 100. },
		  'dried_canned_fish'   : {'calories': 150. },
		  'packaged_fish'       : {'calories': 100. },
		  'fresh_milk'                : {'calories': 40. },
		  'milk_products'       : {'calories': 100. },
		  'canned_milk'         : {'calories': 40. },
	        }

	#graph.write_png('c:/temp/'+str(i)+".png")
	#graph.write_png('file'+str(i)+".png")

def parse_tree(graph,start,t):
	if isinstance(t,dict):
		return [ parse_tree(graph,key,t[key]) for key in t]
	if isinstance(t,list):
		return [parse_tree(graph,start,x) for x in t ]
	if isinstance(t,str):
		graph.add_edge(pydot.Edge(start,t))
		return (start,t)
	raise ValueError("Invalid type: %s" % type(t) )


for i in foodGroups:
    graph = pydot.Dot(graph_type='graph')
    res= parse_tree(graph,"ET",{'ET':{i:foodGroups[i]}})
    graph.write_png('c:/temp/file'+str(i)+".png")
    print( {i:foodGroups[i]} )