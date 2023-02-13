-- types
type Cell = (Int,Int)  

-- Mystate is either Null or S followed by 
-- current location
-- max board bounds
-- array of mine positions
-- array of already visited positions
-- string containing the last action performed
-- previous state
data MyState = Null | S Cell Cell [Cell] [Cell] String MyState deriving (Show,Eq)


-- helpers
removeNull :: [MyState] -> [MyState]
removeNull x = filter (/=Null) x

delete :: Eq a => a -> [a] -> [a]
delete deleted minePos = [ x | x <- minePos, x /= deleted ] 

maxFinder :: [Cell] -> Cell
maxFinder [(x,y)] = (x,y)
maxFinder ((x,y):(x',y'):t) = maxFinder(((if x >= x' then x else x'),(if y >= y' then y else y')):t)

constructSolutionHelper :: MyState -> [String]
constructSolutionHelper Null = []
constructSolutionHelper (S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState) = (constructSolutionHelper prevState) ++ [prevAction]


-- main methods
up :: MyState -> MyState
up(S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState)  
                | x==0 = Null               
                | (elem (x-1,y) alreadyVisited) = Null
                | otherwise = (S (x-1,y) (maxX,maxY) minePos (alreadyVisited++[(x-1,y)]) "up" (S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState))
				   
down :: MyState -> MyState
down(S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState)  
                | x==maxX = Null
                | (elem (x+1,y) alreadyVisited) = Null
                | otherwise = (S (x+1,y) (maxX,maxY) minePos (alreadyVisited++[(x+1,y)]) "down" (S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState))


left :: MyState -> MyState
left(S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState)  
                | y==0 = Null
                | (elem (x,y-1) alreadyVisited) = Null
                | otherwise = (S (x,y-1) (maxX,maxY) minePos (alreadyVisited++[(x,y-1)]) "left" (S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState))			  
					   
right :: MyState -> MyState
right(S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState) 
                | y==maxY = Null
                | (elem (x,y+1) alreadyVisited) = Null
                | otherwise = (S (x,y+1) (maxX,maxY) minePos (alreadyVisited++[(x,y+1)]) "right" (S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState))


collect :: MyState -> MyState
collect(S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState) 
                | not((elem (x,y) minePos)) = Null
                | otherwise = (S (x,y) (maxX,maxY) (delete (x,y) minePos) alreadyVisited "collect" (S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState))	   
					 

nextMyStates :: MyState -> [MyState]
nextMyStates state = removeNull(
                [up(state)
                ,down(state)
                ,left(state)
                ,right(state)
                ,collect(state)]
            )


isGoal :: MyState -> Bool
isGoal (S (x,y) (maxX,maxY) [] alreadyVisited prevAction prevState)=True
isGoal(S (x,y) (maxX,maxY) minePos alreadyVisited prevAction prevState)=False


search :: [MyState] -> MyState
search (x:otherStates) 
            | isGoal x = x
            | otherwise = search (otherStates++(nextMyStates x)) 


constructSolution :: MyState -> [String]
constructSolution state = filter (not . null) (constructSolutionHelper(search [state]))


solve :: Cell -> [Cell] -> [String]
solve startingPos minePos = constructSolution(S startingPos (maxFinder(minePos++[startingPos])) minePos [startingPos] "" Null)

