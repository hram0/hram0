{- This is first attempt at a haskell implementation of HRAM0. Many parts
 - are quite primitive. Much to be improved and extended.
 -
 - Variable / function names follow conventions in specification at
 - https://hram0.github.io.
 -
 - Author: Michael Clear
 - License: MIT
-}

module HRAM.HRAM0 (eval, 
                   load_program,
                   start_run,
                   advance,
                   init_st,
                   Config,
                   MachineState,
                   ProgramState,
                   Sigma,
                   Opcode) where

import Data.Map (member)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Array as Array
import Control.Monad.State
import Data.Bool.HT

data Sigma = STATE_HALT |
             STATE_ERROR |
             STATE_FETCH |
             STATE_WAIT |
             STATE_EXEC_HLT |
             STATE_EXEC_PUT |
             STATE_EXEC_ADD |
             STATE_EXEC_SUB |
             STATE_EXEC_LOD |
             STATE_EXEC_STO |
             STATE_EXEC_BRN |
             STATE_EXEC_CAL |
             STATE_EXEC_RET |
             STATE_EXEC_MAL |
             STATE_EXEC_FRE deriving (Ord,Eq)

data Opcode = HLT | PUT | ADD | SUB | LOD | STO | BRN | CAL | RET | MAL | FRE
  deriving (Eq)

type Memory = Map.Map Int Int

type ProgramData = ([Int], Memory)
type ST = State (Config, MachineState) ProgramData

program_data (PS r m b e s) = (r, m)

data ProgramState = PS {ps_registers :: [Int],
                        ps_memory ::  Memory,
                        ps_blocks :: Map.Map Int Int,
                        ps_heap_start :: Int,
                        ps_call_stack :: [Int]
                       }

data MachineState = MS {ms_sigma :: Sigma,
                        ms_code :: Array.Array Int Int,
                        ms_data :: [Int],
                        ms_t :: (Int, Int, Int),
                        ms_ps :: ProgramState
                       }
                    

{- Configuration
 -
 - |Parameters|
 - rho :- the number of data registers
 - zeta :- gap between allocated blocks (used to detect overflows)
-}
data Config = Config {conf_rho :: Int, conf_zeta :: Int}

{- Opcodes -}

op_hlt :: Int
op_hlt = 0

op_put :: Int
op_put = 1

op_add :: Int
op_add = 2

op_sub :: Int
op_sub = 3

op_lod :: Int
op_lod = 4

op_sto :: Int
op_sto = 5

op_brn :: Int
op_brn = 6

op_cal :: Int
op_cal = 7

op_ret :: Int
op_ret = 8

op_mal :: Int
op_mal = 9

op_fre :: Int
op_fre = 10

int2op x
  | x == op_hlt = Just HLT
  | x == op_put = Just PUT
  | x == op_add = Just ADD
  | x == op_sub = Just SUB
  | x == op_lod = Just LOD
  | x == op_sto = Just STO
  | x == op_brn = Just BRN
  | x == op_cal = Just CAL
  | x == op_ret = Just RET
  | x == op_mal = Just MAL
  | x == op_fre = Just FRE
  | otherwise = Nothing


num_operands op
  | op ==  HLT || op == RET = 0
  | op == FRE || op == CAL = 1
  | op == ADD || op == SUB = 3
  | otherwise = 2



valid_instr :: Config -> Set.Set Int -> [Int] -> Bool
valid_instr _ _ [] = False
valid_instr conf targets (opint:operands) = vi $ int2op opint
  where vi Nothing = False
        vi (Just op) = length(operands) >= num_operands op && voperands op
        voperands = valid_operands conf targets operands

valid_operands _ _ _ HLT  = True -- hlt is valid unconditionally
valid_operands (Config rho _) targets operands op
  | op == FRE = w_reg oper0
  | op == PUT = w_reg oper1
  | op `elem` [ADD, SUB] = r_reg oper0 && r_reg oper2 && w_reg oper2
  | op `elem` [LOD, MAL] = r_reg oper0 && w_reg oper1
  | op == STO = r_reg oper0 && r_reg oper1
  | op == CAL = oper0 `elem` targets
  | op == BRN = r_reg oper0 && oper1 `elem` targets
  | otherwise = False
  where r_reg r = r >= 0 && r < rho
        w_reg r = r >= -2 && r < rho + 1
        oper0 = operands !! 0
        oper1 = operands !! 1
        oper2 = operands !! 2

aggr_targets offset []  = Set.singleton offset
aggr_targets offset (opint:rem) = aggr $ int2op opint
  where aggr (Just op) = Set.insert offset $
          aggr_targets (offset + (num_operands op) + 1) (drop (num_operands op)
                                                         rem)
        aggr Nothing = Set.empty

vprec conf targets [] = False
vprec conf targets (opint:rem) = valid_instr conf targets (opint:rem) &&
                                 vnext (int2op opint)
  where vnext Nothing = False
        vnext (Just op) = length(rem) == num_operands op ||
          vprec conf targets (drop (num_operands op) rem)


valid_code conf c = vprec conf targets c
  where targets = aggr_targets 0 c

zero3 = (0, 0, 0)

zeros num = replicate num 0

replace :: [a] -> [(Int,a)] -> [a]
replace xs opts = map (\(i, x) -> select x (map (\(j, x') -> (i == j, x'))
                                            opts)) $ zip [0..length xs] xs


fetch_operands code addr nopers
  | nopers == 1 = (t0, 0, 0)
  | nopers == 2 =  (t0, t1, 0)
  | nopers == 3 =  (t0, t1, t2)
  | otherwise = zero3
  where t0 = (Array.!) code addr
        t1 = (Array.!) code (addr + 1)
        t2 = (Array.!) code (addr + 2)   

decode HLT = STATE_EXEC_HLT
decode PUT = STATE_EXEC_PUT
decode ADD = STATE_EXEC_ADD
decode SUB = STATE_EXEC_SUB
decode LOD = STATE_EXEC_LOD
decode STO = STATE_EXEC_STO
decode BRN = STATE_EXEC_BRN
decode CAL = STATE_EXEC_CAL
decode RET = STATE_EXEC_RET
decode MAL = STATE_EXEC_MAL
decode FRE = STATE_EXEC_FRE


fetch (Config rho _) (MS STATE_FETCH code dat _ ps @ (PS r m b e s)) =
  handle $ int2op $ ((Array.!) code pc)
  where handle (Just op) = let nopers = num_operands op
                               t' = fetch_operands code (pc + 1) nopers
                               r' = replace r [(rho, pc + 1 + nopers)]
                           in MS (decode op) code dat t' (PS r' m b e s)
        handle Nothing = MS STATE_HALT code dat zero3 ps
        pc = r !! rho

exec_hlt _ (MS _ code dat _ ps) = MS STATE_HALT code dat zero3 ps

exec_put _ (MS STATE_EXEC_PUT code dat (t0, t1, _)
         (PS r m b e s))
          = let r' = replace r [(t1, t0)]
            in MS STATE_FETCH code dat zero3 (PS r' m b e s)

exec_put _ (MS _ code dat _ ps) = MS STATE_HALT code dat zero3 ps

exec_add _ (MS STATE_EXEC_ADD code dat (t0, t1, t2)
         (PS r m b e s))
          = let r' = replace r [(t2, (r !! t0) + (r !! t1))]
            in MS STATE_FETCH code dat zero3 (PS r' m b e s)

exec_add _ (MS _ code dat _ ps) = MS STATE_HALT code dat zero3 ps

exec_sub _ (MS STATE_EXEC_SUB code dat (t0, t1, t2)
         (PS r m b e s))
          = let r' = replace r [(t2, (r !! t1) - (r !! t0))]
            in MS STATE_FETCH code dat zero3 (PS r' m b e s)

exec_sub _ (MS _ code dat _ ps) = MS STATE_HALT code dat zero3 ps


in_mem addr m b = addr `member` m ||
                  or [True | (s, e) <- bl, addr >= s && addr < e]
  where bl = Map.toList b

load addr m b
  | addr `member` m = Map.lookup addr m
  | or [True | (s, e) <- bl, addr >= s && addr < e] = Just 0
  | otherwise = Nothing
  where bl = Map.toList b

exec_lod _ (MS STATE_EXEC_LOD code dat (t0, t1, _)
         ps @ (PS r m b e s))
  | in_mem r_t0 m b = let Just x = load r_t0 m b
                          r' = replace r [(t1, x)]
                      in MS STATE_FETCH code dat zero3 (PS r' m b e s)
  | otherwise = MS STATE_ERROR code dat zero3 ps
  where r_t0 = r !! t0

exec_lod _ (MS _ code dat _ ps) = MS STATE_HALT code dat zero3 ps

exec_sto _ (MS STATE_EXEC_STO code dat (t0, t1, _)
         ps @ (PS r m b e s))
  | in_mem r_t1 m b = let m' = Map.insert r_t1 r_t0 m
                      in MS STATE_FETCH code dat zero3 (PS r m' b e s)
  | otherwise = MS STATE_ERROR code dat zero3 ps
  where r_t0 = r !! t0
        r_t1 = r !! t1

exec_sto _ (MS _ code dat _ ps) = MS STATE_HALT code dat zero3 ps

exec_brn (Config rho _) (MS STATE_EXEC_BRN code dat (t0, t1, _)
         ps @ (PS r m b e s))
  | r_t0 < 0 = let r' = replace r [(rho, r_t1)]
               in MS STATE_FETCH code dat zero3 (PS r' m b e s)
  | otherwise = MS STATE_FETCH code dat zero3 ps
  where r_t0 = r !! t0
        r_t1 = r !! t1

exec_brn _ (MS _ code dat _ ps) = MS STATE_HALT code dat zero3 ps

exec_cal (Config rho _) (MS STATE_EXEC_CAL code dat (t0, t1, _)
         ps @ (PS r m b e s)) =
  let r' = replace r [(rho, r_t0)]
      s' = pc:s
  in MS STATE_FETCH code dat zero3 (PS r' m b e s')
  where r_t0 = r !! t0
        pc = r !! rho

exec_cal _ (MS _ code dat _ ps) = MS STATE_HALT code dat zero3 ps

exec_ret (Config rho _) (MS STATE_EXEC_RET code dat (t0, t1, _)
         ps @ (PS r m b e (ret_addr:s'))) =
                         let r' = replace r [(rho, ret_addr)]
                         in MS STATE_FETCH code dat zero3 (PS r' m b e s')

exec_ret _ (MS _ code dat _ ps) = MS STATE_HALT code dat zero3 ps

exec_mal (Config _ zeta) (MS STATE_EXEC_BRN code dat (t0, t1, _)
         ps @ (PS r m b e s))
  | r_t0 > 0 = let e' = e + r_t0 + zeta
                   b' = Map.insert e e' b
                   r' = replace r [(r_t1, e)]
               in MS STATE_FETCH code dat zero3 (PS r' m b' e' s)
  | otherwise = MS STATE_FETCH code dat zero3 ps
  where r_t0 = r !! t0
        r_t1 = r !! t1

exec_mal _ (MS _ code dat _ ps) = MS STATE_HALT code dat zero3 ps

exec_fre _ (MS STATE_EXEC_FRE code dat (t0, _, _)
         ps @ (PS r m b e s))
  | r_t0 `member` b = let b' = Map.delete r_t0 b
                          m' = Map.restrictKeys m addr_set

                      in MS STATE_FETCH code dat zero3 (PS r m' b e s)
  | otherwise = MS STATE_FETCH code dat zero3 ps
  where r_t0 = r !! t0
        Just end_addr = Map.lookup r_t0 b
        addr_set = Set.difference (Set.fromList $ Map.keys m)
                                  (Set.fromList [r_t0..end_addr])

                  

exec_fre _ (MS _ code dat _ ps) = MS STATE_HALT code dat zero3 ps


fortify (sigma, f) = (sigma, switch)
  where switch conf ms @ (MS s code dat _ ps)
                | s == sigma = f conf ms
                | s == STATE_ERROR = MS STATE_ERROR code dat zero3 ps
                | otherwise = MS STATE_HALT code dat zero3 ps

ftable = [(STATE_EXEC_HLT, exec_hlt),
         (STATE_EXEC_PUT, exec_put),
         (STATE_EXEC_ADD, exec_add),
         (STATE_EXEC_SUB, exec_sub),
         (STATE_EXEC_LOD, exec_lod),
         (STATE_EXEC_STO, exec_sto),
         (STATE_EXEC_BRN, exec_brn),
         (STATE_EXEC_CAL, exec_cal),
         (STATE_EXEC_RET, exec_ret),
         (STATE_EXEC_MAL, exec_mal),
         (STATE_EXEC_FRE, exec_fre)]

delta conf ms @ (MS sigma code dat _ ps)
  | sigma `member` func_map = f conf ms
  | sigma == STATE_FETCH = fetch conf ms
  | otherwise = MS STATE_HALT code dat zero3 ps
  where Just f = Map.lookup sigma func_map
        func_map = Map.fromList $ map fortify ftable

advance :: ST
advance = state adv_f
  where adv_f (conf, ms) = let ms' = delta conf ms
                               ps' = ms_ps ms'
                               m' = ps_memory ps'
                           in (program_data ps', (conf, ms'))

init_st :: Config -> ST
init_st conf @ (Config rho zeta) = do put (conf, (MS STATE_WAIT code_ar [] zero3
                                                  (PS (zeros rho) Map.empty
                                                   Map.empty 0 [])))
                                      return (zeros (rho + 2), Map.empty)
  where code_ar = Array.listArray (0, 0) []


load_program :: [Int] -> [Int] -> ST
load_program code dat = do (conf @ (Config rho _), _) <- get
                           put (conf, (MS STATE_WAIT code_ar dat zero3
                                       (PS (zeros (rho + 2)) Map.empty
                                        Map.empty 0 [])))
                           return (zeros (rho + 2), Map.empty)
  where code_ar = Array.listArray (0, (length code - 1)) code

start_run :: [Int] -> ST
start_run x = do (conf @ (Config rho zeta), ms) <- get
                 let (MS _ code dat _ _) = ms
                 let static_data = dat ++ x
                 let d = length static_data
                 let r' = zeros rho ++ [0, n]                 
                 let m' = Map.fromList $ zip [0..(d - 1)] static_data
                 let e' = d + zeta
                 put (conf, MS STATE_FETCH code dat zero3
                       (PS r' m' Map.empty e' []))
                 return (r', m')
  where n = length x

exec_until_halt :: ST
exec_until_halt = do (conf, ms) <- get
                     let (MS sigma _ _ _ (PS r m _ _ _)) = ms
                     if sigma == STATE_HALT || sigma == STATE_ERROR
                       then return (r, m)
                       else do
                           advance
                           exec_until_halt

eval :: Config -> [Int] -> [Int] -> [Int] -> ST
eval conf code dat x = do init_st conf
                          load_program code dat
                          start_run x
                          advance
                          exec_until_halt

