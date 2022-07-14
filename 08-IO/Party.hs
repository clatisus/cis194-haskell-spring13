{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where
import Employee (Employee(..), GuestList(..), testCompany)
import Data.Tree (Tree(..))
import Data.List (sort)

-------------------------------------------------------------------------------
-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e@Emp { empFun = x }  (GL es f) = GL (e:es) (f + x)

instance Semigroup GuestList where
  (<>) (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-------------------------------------------------------------------------------
-- Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node { rootLabel = r, subForest = sf } = f r (map (treeFold f) sf)

-------------------------------------------------------------------------------
-- Exercise 3

--            boss  |  with subBoss, without subBoss | with boss, without boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b gls = (
                    glCons b (mconcat $ map snd gls),
                    mconcat $ map (uncurry moreFun) gls
                  )

-------------------------------------------------------------------------------
-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

testCompanyGl :: GuestList
testCompanyGl = maxFun testCompany

-- GL [Emp {empName = "John", empFun = 1},Emp {empName = "Sue", empFun = 5},Emp {empName = "Fred", empFun = 3},Emp {empName = "Sarah", empFun = 17}] 26

-------------------------------------------------------------------------------
-- Exercise 5

main :: IO()
main = do
  fileContent <- readFile "company.txt"
  let (GL es fun) = maxFun . read $ fileContent
  putStrLn $ "Total fun: " ++ show fun
  putStrLn . unlines . sort . map empName $ es

-- λ> main
-- Total fun: 33200
-- Adam Bohrmann
-- Adam Briscoe
-- ...
