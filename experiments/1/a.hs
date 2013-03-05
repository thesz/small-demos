import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

type Coef = Double

data E =
                V       Int
        |       C       Coef
        |       B       BOp     E       E
        |       U       UF      E
        deriving (Eq, Ord, Show)

data BOp = Plus | Minus | Mul | Div
        deriving (Eq, Ord, Show)

data UF = Neg | Sin | Cos | Sqrt | Exp
        deriving (Eq, Ord, Show)

type Index = Int

data E' =
                V'      Int
        |       C'      Coef
        |       B'      BOp     Index   Index
        |       U'      UF      Index

-------------------------------------------------------------------------------
-- Handy operators.

infixl 6 .+, .-
(.+), (.-) :: E -> E -> E
a .+ b = B Plus a b
a .- b = B Minus a b

neg :: E -> E
neg = U Neg

-------------------------------------------------------------------------------
-- Defining the systems.

data Point =
                Point           Int
        |       FixedVoltage    Coef

data Scheme = Scheme {
          schUnique             :: Int                  -- unique index.
        , schPointCurrent       :: Map.Map Point [E]    -- list of vars of currents.
        , schPointVoltage       :: Map.Map Point E      -- the voltage expression (variable).
        , schEquality           :: [E]                  -- list of expressions that are equal to zero.
        }

emptyScheme :: Scheme
emptyScheme = Scheme {
          schUnique             = 0
        , schPointCurrent       = Map.empty
        , schPointVoltage       = Map.empty
        , schEquality           = []
        }

type SchM a = State Scheme a

smUnique :: SchM Int
smUnique = modify (\s -> s { schUnique = schUnique s + 1}) >> liftM schUnique get

point :: SchM Point
point = do
        i <- smUnique
        modify $ \s -> s {
                  schPointCurrent = Map.insert i [] $ schPointCurrent s
                , schPointVoltage = Map.insert i (V i) $ schPointVoltage s
                }
        return $ Point i

connection :: Point -> Point -> (E -> E -> SchM ()) -> SchM ()
connection a b act = do
        u <- liftM2 (.-) (getVoltage a) (getVoltage b)
        i <- newCurrent
        modify $ \s -> s {
                  schPointCurrent = Map.insertWith (++) a [i] $ Map.insertWith (++) b [neg i] $ schPointCurrent s
                }
        return ()
        where
                newCurrent = liftM V smUnique
                getVoltage i = liftM (Map.findWithDefault (error "no voltage!") i . schPointVoltage) get

ground :: Point
ground = FixedVoltage 0

fixedVoltage :: Coef -> Point
fixedVoltage = FixedVoltage

-------------------------------------------------------------------------------
-- Testing.

