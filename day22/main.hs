import Text.Parsec hiding (State)
import Text.Parsec.String (parseFromFile)
import Data.Maybe (fromJust)
import Control.Monad
import Control.Applicative (liftA2)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Maybe

data Character = Character { nick :: String, baseStats :: Stats } deriving (Show, Eq)
data Stats = Stats { hp, damage, armor, mana :: Int } deriving (Show, Eq)
data EffectType = Shield | Poison | Recharge deriving (Show, Eq)
data Effect = Effect { from :: EffectType, ticksLeft :: Int} deriving Show
data Game = Game { c1, c2 :: Character, effects :: [Effect], manaUse :: Int } deriving Show

type Move a = MaybeT (StateT Game []) a

instance Monoid Stats where
  mempty = Stats { hp = 0, damage = 0, armor = 0, mana = 0 }
  s1 `mappend` s2 = Stats { hp     = hp     s1 + hp     s2
                          , damage = damage s1 + damage s2
                          , armor  = armor  s1 + armor  s2
                          , mana   = mana   s1 + mana   s2
                          }

parseNumber :: Parsec String u Int
parseNumber = read <$> many1 digit

parseInput :: String -> Parsec String u Character
parseInput n = Character n <$> parseStats <* eof where
  parseStats = (\h d -> mempty {hp = h, damage = d}) <$> the "Hit Points" <*> the "Damage"
  the s      = string (s ++ ": ") *> parseNumber <* endOfLine

magicMissile , drain, shield, poison, recharge :: Move()
spells :: Move()
spells = join $ (lift.lift) [magicMissile , drain, shield, poison, recharge]

magicMissile = useMana 53 >> damageBoss 4

drain = useMana 73 >> damagePlayer (-2) >> damageBoss 2

shield = do
  useMana 113
  armorizePlayer 7
  addEffect Effect { from = Shield, ticksLeft = 6}

poison = do
  useMana 173
  addEffect Effect { from = Poison, ticksLeft = 6}

recharge = do
  useMana 229
  addEffect Effect { from = Recharge, ticksLeft = 5}

useMana :: Int -> Move()
useMana n = ensureMana n >> addMana (-n) >> registerMana n

getStat :: (Game -> Character) -> (Stats -> Int) -> Move Int
getStat c f = lift $ gets (f . baseStats . c)

ensureMana :: Int -> Move()
ensureMana n = do
  playerMana <- getStat c1 mana
  unless (playerMana >= n) loose

registerMana :: Int -> Move()
registerMana n = lift $ modify (\g -> g { manaUse = manaUse g + n })

modifyPlayerStats :: (Stats -> Stats) -> Move()
modifyPlayerStats f = lift $ modify (\g -> g { c1 = (c1 g) { baseStats = f (baseStats $ c1 g) } })

modifyBossStats :: (Stats -> Stats) -> Move()
modifyBossStats f = lift $ modify (\g -> g { c2 = (c2 g) { baseStats = f (baseStats $ c2 g) } })

-- | Change mana of player
addMana :: Int -> Move()
addMana n = modifyPlayerStats (\s -> s { mana = mana s + n })

-- | change armor of player
armorizePlayer :: Int -> Move()
armorizePlayer n = modifyPlayerStats (\s -> s { armor = armor s + n })

-- | change hp of player
damagePlayer :: Int -> Move()
damagePlayer n = do
  modifyPlayerStats (\s -> s { hp = hp s - n })
  playerHp <- getStat c1 hp
  when (playerHp <= 0) loose

loose :: Move()
loose = lift $ lift []

-- | change hp of boss
damageBoss :: Int -> Move()
damageBoss n = do
  modifyBossStats (\s -> s { hp = hp s - n })
  bossHp <- getStat c2 hp
  when (bossHp <= 0) win

win :: Move()
win = fail "Le boss is dead!"

-- | check if effect is active, add effect to list
addEffect :: Effect -> Move()
addEffect e = do
  es <- lift $ gets effects
  if from e `elem` map from es
    then loose
    else lift $ modify (\g -> g { effects = e:es })

-- | Play rounds untill game ends
rounds :: Bool -> Move()
rounds hard = do
  when hard (damagePlayer 1)
  playerAction
  bossAction
  rounds hard

playerAction :: Move()
playerAction = handleEffects >> spells

bossAction :: Move()
bossAction = handleEffects >> bossAttack

bossAttack :: Move()
bossAttack = calcBossDamage >>= damagePlayer

calcBossDamage :: Move Int
calcBossDamage = fightDamage <$> lift (gets (baseStats . c2)) <*> lift (gets (baseStats . c1))

handleEffects :: Move()
handleEffects = applyEffects >> wearOffEffects

-- | decrease ticks of each effect and call applyEffect
applyEffects :: Move()
applyEffects = do
  es <- lift $ gets effects
  forM_ es $ applyEffect . from
  lift $ modify (\g -> g { effects = map (\e -> e { ticksLeft = pred $ ticksLeft e }) es })

-- | Effect moves executed for every round the effect is active
applyEffect :: EffectType -> Move()
applyEffect Poison   = damageBoss 3
applyEffect Recharge = addMana 101
applyEffect _        = return ()

-- | remove the effect from the list and call wearOffEffect
wearOffEffects :: Move()
wearOffEffects = do
  es <- lift $ gets effects
  forM_ (filter (not . active) es) $ wearOffEffect . from
  lift $ modify (\g -> g { effects = filter active es })

active :: Effect -> Bool
active = (>0) . ticksLeft

-- | Effect moves executed when the effect wars off
wearOffEffect :: EffectType -> Move()
wearOffEffect Shield = armorizePlayer (-7)
wearOffEffect _      = return ()

fightDamage :: Stats -> Stats -> Int
fightDamage s1 s2 = max 1 $ damage s1 - armor s2

manaNeeded :: Bool -> Character -> Character -> Int
manaNeeded hard pl bo = minimum $ map manaUse $ execStateT (runMaybeT (rounds hard)) Game { c1 = pl, c2 = bo, effects = [], manaUse = 0 }

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

main :: IO()
main = mainP2 where
  mainP1 = main' False
  mainP2 = main' True
  main' hard = do
    boss       <- fromRight <$> parseFromFile (parseInput "Boss") "input.txt"
    let player =  Character { nick = "Henry Case", baseStats = mempty { hp = 50, mana = 500 } }

    print $ manaNeeded hard player boss
