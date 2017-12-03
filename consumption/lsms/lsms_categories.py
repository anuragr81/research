import pydot

expenseTree= {
'intoxication' :['cigarettes','beer','brews','winespirits'],
'food': {'beverages':['water','tea','coffee','miscdrinkpowder','canned_drink','readymade_tea','readymade_coffee',],
         'starch': ['milling','rice_husked','rice_paddy','maize_green','maize_grain','maize_flour','millet_grain','millet_flour','wheat','bread','bunscakes','pasta','othercereal','pulses',] , 
         'vegstarch' : ['cassava_fresh','cassava_flour','sweet_potato','yam','potatoes','banana_green','othervegstarch'], 
         'sugars':['sugar','sweet','honey'], 
         'fat':['peanuts','coconut','cashew_almonds','nut_products','cooking_oil','butter_margarine',],
         'vegetables':['onion','greens','dried_canned_veg',] ,
         'fruits':['banana_ripe','citrus','mangoes','sugarcane'],
         'protein':['goat','beef','pork','chicken','wild_birds','wild_meat','fish_seafood','dried_canned_fish','packaged_fish','milk','milk_products','canned_mik'], 
         'condiments': ['spices','salt'] },
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
}


	#graph.write_png('c:/temp/'+str(i)+".png")
	#graph.write_png('file'+str(i)+".png")

def parse_tree(start,t):
	if isinstance(t,dict):
		return [ parse_tree(key,t[key]) for key in t]
	if isinstance(t,list):
		return [parse_tree(start,x) for x in t ]
	if isinstance(t,str):
		graph.add_edge(pydot.Edge(start,t))
		return (start,t)
	raise ValueError("Invalid type: %s" % type(t) )

res= parse_tree("ET",{'ET':expenseTree})

#print parse_tree("ET",{"one":"One"})
#graph.write_png('c:/temp/test.png')