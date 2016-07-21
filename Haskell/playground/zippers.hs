import qualified Data.List as L

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

freeTree :: Tree Char  
freeTree =
  Node 'P'
    (Node 'O'
      (Node 'L'
        (Node 'N' Empty Empty)
        (Node 'T' Empty Empty)
      )
      (Node 'Y'
        (Node 'S' Empty Empty)
        (Node 'A' Empty Empty)
      )
    )
    (Node 'L'
      (Node 'W'
        (Node 'C' Empty Empty)
        (Node 'R' Empty Empty)
      )
      (Node 'A'
        (Node 'A' Empty Empty)
        (Node 'C' Empty Empty)
      )
    )

data Direction = L | R deriving Show
type Directions = [Direction]

changeToP                     :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) =  Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) =  Node x l (changeToP ds r)
changeToP []     (Node _ l r) =  Node 'P' l r

elemAt                     :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) =  elemAt ds l
elemAt (R:ds) (Node _ _ r) =  elemAt ds r
elemAt []     (Node x _ _) =  x

type Breadcrumbs a = [Crumb a]

goLeft                  :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) =  Just (l, (LeftCrumb x r):bs)
goLeft (Empty, _)       =  Nothing

goRight                  :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) =  Just (r, (RightCrumb x l):bs)
goRight (Empty, _)       =  Nothing

goUp                            :: Zipper a -> Maybe (Zipper a)
goUp ( n, (LeftCrumb a t):bs )  =  Just (Node a n t, bs)
goUp ( n, (RightCrumb a t):bs ) =  Just (Node a t n, bs)
goUp ( _, [] )                  =  Nothing

(-:)   :: a -> (a -> b) -> b
x -: f =  f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving Show

type Zipper a = (Tree a, Breadcrumbs a)

modify                    :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) =  (Node (f x) l r, bs)
modify f (Empty, bs)      =  (Empty, bs)

attach           :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) =  (t, bs)

topMost         :: Zipper a -> Zipper a
topMost (t, []) =  (t, [])
topMost z       =  topMost $ goUp z

type ListZipper a = ([a], [a])

goForward            :: ListZipper a -> ListZipper a
goForward (x:xs, bs) =  (xs, x:bs)

goBack            :: ListZipper a -> ListZipper a
goBack (xs, b:bs) =  (b:xs, bs)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving Show

myDisk :: FSItem  
myDisk = 
  Folder "root"   
    [ File "goat_yelling_like_man.wmv" "baaaaaa"  
    , File "pope_time.avi" "god bless"  
    , Folder "pics"  
      [ File "ape_throwing_up.jpg" "bleargh"  
      , File "watermelon_smash.gif" "smash!!"  
      , File "skull_man(scary).bmp" "Yikes!"  
      ]  
    , File "dijon_poupon.doc" "best mustard"  
    , Folder "programs"  
      [ File "fartwizard.exe" "10gotofart"  
      , File "owl_bandit.dmg" "mov eax, h00t"  
      , File "not_a_virus.exe" "really not a virus"  
      , Folder "source code"  
        [ File "best_hs_prog.hs" "main = print (fix error)"  
        , File "random.hs" "main = print 4"  
        ]  
      ]  
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving Show
type FSZipper = (FSItem, [FSCrumb])

fsUp                                 :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name ls rs):bs) =  (Folder name $ ls ++ [item] ++ rs, bs)

fsTo                                    :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, target:rs) = L.break (isName name) items
  in  (target, (FSCrumb folderName ls rs):bs)

isName                       :: String -> FSItem -> Bool
isName name (File fName _)   =  name == fName
isName name (Folder fName _) =  name == fName

fsRename                                 :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) =  (Folder newName items, bs)
fsRename newName (File name dat, bs)     =  (File newName dat, bs)

fsNewFile                              :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder name items, bs) =  (Folder name (item:items), bs)
