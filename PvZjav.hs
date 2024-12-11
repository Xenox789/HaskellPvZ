module Base where

type Coordinate = (Int, Int)

type Sun = Int

data Plant = Peashooter Int | Sunflower Int | Walnut Int | CherryBomb Int
    deriving (Eq,Show)

data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int
    deriving (Eq, Show) 

data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)]
    deriving (Eq, Show)


defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2


price :: Plant -> Int
price xs
    | xs == defaultPeashooter = 100
    | xs == defaultSunflower || xs == defaultWalnut = 50
    | xs == defaultCherryBomb = 150

tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel sun plants zombies) (a,b) xs 
    | price xs <= sun && lookup (a,b) plants == Nothing && a < 5 && b < 12 && a >= 0 && b >= 0 = Just (GameModel (sun - price xs) ([((a,b), xs)] ++ plants) zombies)
    | otherwise = Nothing 


placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel sun plants zombies) ys x 
    | lookup (x,11) zombies == Nothing && x < 5 && x >= 0 = Just (GameModel sun plants ([((x,11), ys)] ++ zombies))
    | otherwise = Nothing



---------------------------------------------------------------------------------------------------------------

performZombieActions :: GameModel -> Maybe GameModel
performZombieActions (GameModel sun plants zombies) = if [] `elem` newzombies then Nothing else Just (GameModel sun newplants (concat $ newzombies))
    where   newplants = map (\x -> ppActions x zombies) plants
            newzombies = map (\x -> pzActions x plants) zombies



ppActions :: (Coordinate, Plant) -> [((Coordinate), Zombie)] -> (Coordinate, Plant)
ppActions ((a,b), (Peashooter x)) zombies = if (lookup (a,b) $ filter (not.isfastVaulting) zombies) == Nothing then ((a,b), (Peashooter x)) else ((a,b), (Peashooter (x-(damage (a,b) $ filter (not.isfastVaulting) zombies))))
ppActions ((a,b), (Sunflower x)) zombies = if (lookup (a,b) $ filter (not.isfastVaulting) zombies) == Nothing then ((a,b), (Sunflower x)) else ((a,b), (Sunflower (x-(damage (a,b) $ filter (not.isfastVaulting) zombies))))
ppActions ((a,b), (Walnut x)) zombies = if (lookup (a,b) $ filter (not.isfastVaulting) zombies) == Nothing then ((a,b), (Walnut x)) else ((a,b), (Walnut (x-(damage (a,b) $ filter (not.isfastVaulting) zombies))))
ppActions ((a,b), (CherryBomb x)) zombies = if (lookup (a,b) $ filter (not.isfastVaulting) zombies) == Nothing then ((a,b), (CherryBomb x)) else ((a,b), (CherryBomb (x-(damage (a,b) $ filter (not.isfastVaulting) zombies))))


pzActions :: (Coordinate, Zombie) -> [((Coordinate), Plant)] -> [(Coordinate, Zombie)]
pzActions ((a,b), Vaulting x y) plants
    | isAPlantAhead plants ((a,b), Vaulting x y) == False && (b-y) < 0 = []
    | isAPlantAhead plants ((a,b), Vaulting x y) == False && isAPlantAhead plants ((a,(b-1)), Vaulting x y) && y == 2 = [((a,(b-2)), Vaulting x (y-1))]
    | isAPlantAhead plants ((a,b), Vaulting x y) == False && isAPlantAhead plants ((a,(b-1)), Vaulting x y) == False && y == 2 = [((a,(b-2)), Vaulting x y)]
    | isAPlantAhead plants ((a,b), Vaulting x y) == False = [((a,(b-1)), Vaulting x y)] --Lehet hiba a lépésnél
    | isAPlantAhead plants ((a,b), Vaulting x y) && y == 2 = [((a,(b-1)), Vaulting x (y-1))]
    | otherwise = [((a,b), Vaulting x y)]
pzActions ((a,b), x) plants
    | isAPlantAhead plants ((a,b), x) == False && b==0 = []
    | isAPlantAhead plants ((a,b), x) == False = [((a, (b-1)), x)]
    | otherwise = [((a,b), x)]


damage (a,b) [] = 0
damage (a,b) (z:zombies)
    | (a,b) == fst z = 1 + damage (a,b) zombies
    | otherwise = damage (a,b) zombies


isfastVaulting :: (Coordinate, Zombie) -> Bool
isfastVaulting ((a,b), x)
    | x == Vaulting 1 2 = True
    | x == Vaulting 2 2 = True
    | x == Vaulting 3 2 = True
    | x == Vaulting 4 2 = True
    | x == Vaulting 5 2 = True
    | x == Vaulting 6 2 = True
    | x == Vaulting 7 2 = True
    | otherwise = False

isAPlantAhead :: [(Coordinate, Plant)] -> (Coordinate, Zombie) -> Bool
isAPlantAhead plants ((a,b), x)
    | lookup (a,b) plants == Nothing = False
    | otherwise = True


----------------------------------------------------------------------------------------------------------------------------------------------------------


cleanBoard :: GameModel -> GameModel
cleanBoard (GameModel sun plants zombies) = (GameModel sun newplants newzombies)
     where   newplants = deletePlants (GameModel sun plants zombies)
             newzombies = deleteZombies (GameModel sun plants zombies)


deletePlants :: GameModel -> [(Coordinate, Plant)]
deletePlants (GameModel sun plants zombies) = concat $ map (deletePlant) plants

deletePlant :: (Coordinate, Plant) -> [(Coordinate, Plant)]
deletePlant ((a,b), (Peashooter x)) = if x <= 0 then [] else [((a,b), Peashooter x)]
deletePlant ((a,b), (Sunflower x)) = if x <= 0 then [] else [((a,b), Sunflower x)]
deletePlant ((a,b), (Walnut x)) = if x <= 0 then [] else [((a,b), Walnut x)]
deletePlant ((a,b), (CherryBomb x)) = if x <= 0 then [] else [((a,b), CherryBomb x)]



deleteZombies :: GameModel -> [(Coordinate,Zombie)]
deleteZombies (GameModel sun plants zombies) = concat $ map (deleteZombie) zombies


deleteZombie :: (Coordinate,Zombie) -> [(Coordinate,Zombie)]
deleteZombie ((a,b),(Basic x y)) = if x <= 0 then [] else [((a,b),(Basic x y))]
deleteZombie ((a,b),(Conehead x y)) = if x <= 0 then [] else [((a,b),(Conehead x y))]
deleteZombie ((a,b),(Buckethead x y)) = if x <= 0 then [] else [((a,b),(Buckethead x y))]
deleteZombie ((a,b),(Vaulting x y)) = if x <= 0 then [] else [((a,b),(Vaulting x y))]