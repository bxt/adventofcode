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
data Game = Game { player, boss :: Character, effects :: [Effect], manaUse :: Int } deriving Show

type Move a = MaybeT (StateT Game []) a

instance Monoid Stats where
  mempty = Stats { hp = 0, damage = 0, armor = 0, mana = 0 }
  s1 `mappend` s2 = Stats { hp     = hp     s1 + hp     s2
                          , damage = damage s1 + damage s2
                          , armor  = armor  s1 + armor  s2
                          , mana   = mana   s1 + mana   s2
                          }

parseInput :: String -> Parsec String u Character
parseInput n = Character n <$> parseStats <* eof where
  parseStats = (\h d -> mempty { hp = h, damage = d }) <$> the "Hit Points" <*> the "Damage"
  the s      = string (s ++ ": ") *> parseNumber <* endOfLine
  parseNumber = read <$> many1 digit

loose, win :: Move()
loose = lift $ lift []
win = fail "Le boss is dead!"

getStat :: (Game -> Character) -> (Stats -> a) -> Move a
getStat c f = lift $ gets (f . baseStats . c)

appendPlayerStats :: Stats -> Move()
appendPlayerStats s = lift $ modify (\g -> g { player = (player g) { baseStats = s `mappend` baseStats (player g) } })

appendBossStats :: Stats -> Move()
appendBossStats s = lift $ modify (\g -> g { boss = (boss g) { baseStats = s `mappend` baseStats (boss g) } })

-- | change hp of player, check for death
damagePlayer :: Int -> Move()
damagePlayer n = do
  appendPlayerStats $ mempty { hp = -n }
  playerHp <- getStat player hp
  when (playerHp <= 0) loose

-- | change hp of boss, check for death
damageBoss :: Int -> Move()
damageBoss n = do
  appendBossStats $ mempty { hp = -n }
  bossHp <- getStat boss hp
  when (bossHp <= 0) win

addPlayerMana :: Int -> Move()
addPlayerMana n = appendPlayerStats $ mempty { mana = n }

armorizePlayer :: Int -> Move()
armorizePlayer n = appendPlayerStats $ mempty { armor = n }

spells :: Move()
spells = join $ (lift.lift) [magicMissile , drain, shield, poison, recharge] where
  magicMissile , drain, shield, poison, recharge :: Move()
  magicMissile = useMana 53 >> damageBoss 4
  drain = useMana 73 >> damagePlayer (-2) >> damageBoss 2
  shield = useMana 113 >> armorizePlayer 7
        >> addEffect Effect { from = Shield, ticksLeft = 6}
  poison = useMana 173 >> addEffect Effect { from = Poison, ticksLeft = 6}
  recharge = useMana 229 >> addEffect Effect { from = Recharge, ticksLeft = 5}

  useMana :: Int -> Move()
  useMana n = ensureMana n >> addPlayerMana (-n) >> registerMana n where
    ensureMana n = do
      playerMana <- getStat player mana
      unless (playerMana >= n) loose
    registerMana n = lift $ modify (\g -> g { manaUse = manaUse g + n })

setEffects :: [Effect] -> Move()
setEffects es = lift $ modify (\g -> g { effects = es })

-- | check if effect is active, add effect to list
addEffect :: Effect -> Move()
addEffect e = do
  es <- lift $ gets effects
  if from e `elem` map from es
    then loose
    else setEffects $ e:es

-- | decrease ticks of each effect and apply effect
applyEffects :: Move()
applyEffects = do
  es <- lift $ gets effects
  forM_ es $ applyEffect . from
  setEffects $ map (\e -> e { ticksLeft = pred $ ticksLeft e }) es
  where
    applyEffect :: EffectType -> Move()
    applyEffect Poison   = damageBoss 3
    applyEffect Recharge = addPlayerMana 101
    applyEffect _        = return ()

-- | remove the effect from the list and handle wear off
wearOffEffects :: Move()
wearOffEffects = do
  es <- lift $ gets effects
  forM_ (filter (not . active) es) $ wearOffEffect . from
  setEffects $ filter active es
  where
    active :: Effect -> Bool
    active = (>0) . ticksLeft
    wearOffEffect :: EffectType -> Move()
    wearOffEffect Shield = armorizePlayer (-7)
    wearOffEffect _      = return ()

calcBossDamage :: Move Int
calcBossDamage = fightDamage <$> getStat boss id <*> getStat player id where
  fightDamage :: Stats -> Stats -> Int
  fightDamage s1 s2 = max 1 $ damage s1 - armor s2

-- | Play rounds until game ends
rounds :: Bool -> Move()
rounds hard = do
  when hard (damagePlayer 1)
  playerAction
  bossAction
  rounds hard
  where
    playerAction = handleEffects >> spells
    bossAction = handleEffects >> bossAttack
    bossAttack = calcBossDamage >>= damagePlayer
    handleEffects = applyEffects >> wearOffEffects

games :: Bool -> Character -> Character -> [Game]
games hard pl bo = execStateT (runMaybeT (rounds hard)) start where
  start = Game { player = pl, boss = bo, effects = [], manaUse = 0 }

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

main :: IO()
main = mainP2 where
  mainP1 = main' False
  mainP2 = main' True
  main' hard = do
    boss       <- fromRight <$> parseFromFile (parseInput "Boss") "input.txt"
    let player =  Character { nick = "Henry Case", baseStats = mempty { hp = 50, mana = 500 } }

    print $ minimum $ map manaUse $ games hard player boss
