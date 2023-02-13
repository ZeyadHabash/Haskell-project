type Cell = (Int,Int)  
data MyState = Null | S Cell [Cell] String MyState  deriving (Show,Eq)

up :: MyState -> MyState
up(S (x,y) minePos prevAction prevState)  
                | x==0 = Null               
                | otherwise = (S (x-1,y) minePos "up" (S (x,y) minePos prevAction prevState))
				   
down :: MyState -> MyState
down(S (x,y) minePos prevAction prevState)  
                | x==3 = Null
                | otherwise = (S (x+1,y) minePos "down" (S (x,y) minePos prevAction prevState))


left :: MyState -> MyState
left(S (x,y) minePos prevAction prevState)  
                | y==0 = Null
                | otherwise = (S (x,y-1) minePos "left" (S (x,y) minePos prevAction prevState))			  
					   
right :: MyState -> MyState
right(S (x,y) minePos prevAction prevState) 
                | y==3 = Null
                | otherwise = (S (x,y+1) minePos "right" (S (x,y) minePos prevAction prevState))

delete :: Eq a => a -> [a] -> [a]
delete deleted minePos = [ x | x <- minePos, x /= deleted ] 

collect :: MyState -> MyState
collect(S (x,y) minePos prevAction prevState) 
                | not((elem (x,y) minePos)) = Null
                |otherwise = (S (x,y) (delete (x,y) minePos) "collect" (S (x,y) minePos prevAction prevState))	   
					 

nextMyStates :: MyState -> [MyState]
nextMyStates state = removeNull(
                [up state
                ,down state
                ,left state
                ,right state
                ,collect state]
            )

removeNull :: [MyState] -> [MyState]
removeNull x = filter (/=Null) x


isGoal :: MyState -> Bool
isGoal (S (x,y) [] prevAction prevState)=True
isGoal(S (x,y) minePos prevAction prevState)=False

search :: [MyState] -> MyState
search (x:otherStates) 
            | isGoal x = x
            | otherwise = search (otherStates++(nextMyStates x)) 


constructSolutionHelper :: MyState -> [String]
constructSolutionHelper Null = []
constructSolutionHelper (S (x,y) minePos prevAction prevState) = (constructSolutionHelper prevState) ++ [prevAction]

constructSolution :: MyState -> [String]
constructSolution state = filter (not . null) (constructSolutionHelper(search [state]))


solve :: Cell -> [Cell] -> [String]
solve startingPos minePos = constructSolution(S startingPos minePos "" Null)
            
