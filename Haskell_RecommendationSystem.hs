import System.Random
import System.IO.Unsafe

users = ["user1","user2" , "user3" , "user4"]
items = ["item1","item2","item3","item4","item5","item6"]
purchasesHistory = [("user1",[["item1","item2","item3"],["item1","item2","item4"]]),("user2",[["item2","item5"],["item4","item5"]]),("user3",[["item3","item2"]]),("user4",[])]

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList [] = []
createEmptyFreqList (i:items) = (i,[]) : (createEmptyFreqList items)

getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats ((u,userItems):xs) = helperStats (createEmptyFreqList items) ((u,userItems):xs)  



userItem (x,list) (u,userItems)= (x,items) where items= itematRemZero (itemat x (distinct userItems) userItems)

allUserItems [] _ = []
allUserItems ((x,list):xs) (u,items) = [userItem (x,list) (u,items)] ++ (allUserItems xs (u,items)) 

userAdd ((x,list):xs) (u,items) = (u,allUserItems ((x,list):xs) (u,items))

helperStats _ [] =[]
helperStats ((x,list):xs) ((u,items):ys) = [userAdd ((x,list):xs) (u,items)] ++ (helperStats ((x,list):xs) ys)



countPair currItem outItem list =(outItem ,count currItem outItem list) 
--getCountPair currItem outItem list = countPair currItem outItem list
--countPairHelp(x,y) = if(y==0) then countPairZero 
					  --else (x,y)

count currItem outItem [] = 0
count currItem outItem (i:items) = if ismemeber currItem i && ismemeber outItem i then 1+(count currItem outItem items) else (count currItem outItem items)  

itemathelper y [] list=[]
itemathelper y (x:xs) list =if(y/=x) then ( countPair y x list): (itemathelper y xs list) else itemathelper y xs list 

itemat curritem itemsDis list =  dis (itemathelper curritem itemsDis list ) 

itematRemZero [] = [] 
itematRemZero ((x,y):xs) = if (y==0) then itematRemZero xs
							else (x,y): (itematRemZero xs)



flatten[]=[]
flatten (x:xs)= x ++ (flatten xs) 

dis :: Eq a => [a] -> [a]
dis [] =[] 
dis (x:xs)|ismemeber x xs=dis xs
		  |otherwise=(x:dis xs)
ismemeber x []=False
ismemeber x (y:ys)|x==y=True
				  |otherwise=ismemeber x ys 

distinct []=[]
distinct list= dis (flatten list)

sort [] =[] 

removeF (x:xs) = xs
getNum list = removeF (removeF (removeF (removeF list) ))

toNum (x:xs) = x ++ (toNum xs)

-----------freqListItems------------

freqListItems user = (occInUserStatsAllItems items userItems) where userItems = (getItemsOfUserUS user (getAllUsersStats purchasesHistory))

getItemsOfUserUS user ((u,items):xs) = if (user==u) then items else getItemsOfUserUS user xs
		
getBindingsOfItem item ((i,bindings):xs) = if (item == i) then bindings 
									  else getBindingsOfItem item xs

getFreqOfBindedItem _ [] = 0									  
getFreqOfBindedItem item ((i,n):xs) = if(item==i) then n else getFreqOfBindedItem item xs

occInUserStats _ [] =0
occInUserStats item ((i,bindings):xs) = if (item/=i) then (getFreqOfBindedItem item bindings) + (occInUserStats item xs)
									else (occInUserStats item xs) 

occInUserStatsPair item ((i,bindings):xs)= (item,(occInUserStats item ((i,bindings):xs)))
			
occInUserStatsAllItems items userItems = itematRemZero (occInUserStatsAllItemsHelp items userItems)

occInUserStatsAllItemsHelp [] _ =[]			
occInUserStatsAllItemsHelp (i:is) userItems = [occInUserStatsPair i userItems ] ++ (occInUserStatsAllItemsHelp is userItems) 

-----------------------------------------------------------------									
 									  
nOfOcc item [] = 0									
nOfOcc item (i:itemsL) = if (item==i) then 1 + (nOfOcc item itemsL)
						else nOfOcc item itemsL	

nOfOccPair item itemsList = (item,(nOfOcc item itemsList))		

--getItemsOfUserPH _ [] =[] 
--getItemsOfUserPH user ((u,list):xs) = if (user==u) then list 
								--	else (getItemsOfUserPH user xs)
									
--getItemsOfUserPHF user ((u,list):xs) = flatten (getItemsOfUserPH user ((u,list):xs)	)							
		
				
freqListItemsHelp [] _ =[]				
freqListItemsHelp (y:ys) listItems= if (ismemeber y ys) then (freqListItemsHelp ys listItems)
									else [nOfOccPair y listItems] ++ (freqListItemsHelp ys listItems)