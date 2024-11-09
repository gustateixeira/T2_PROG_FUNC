--Definição dos tipos


type Reg = Int

--LOD
execLOD :: Int -> ([(Int,Int)], Reg, Bool) -> ([(Int,Int)], Int, Bool)
execLOD end (mem, acc, eqz) = (mem, readMem mem end, eqz)



--JMP
-- readMem [(0,10),(1,3),(2,23),(10,100)] 1 = 3
-- fst -> primeiro elemento da tupla

-- snd -> segundo elemento da tupla
readMem :: [(Int,Int)] -> Int -> Int
readMem (m:ms) end  
    | end == fst m = snd m
    | end /= fst m = readMem ms end    


--Escrita da memória
execSto :: [(Int,Int)] -> Int -> Reg -> [(Int,Int)]
execSto (m:ms) end acc | end == fst m = [(fst m, acc)] ++ ms
                       | end /= fst m = m : execSto ms end acc


execJmp :: Int -> Int ->Int
execJmp end pc = if pc /= end then execJmp end (pc+2) 
                else pc 
      
-- [(Instruções, funções)] -> retorna função

execJmz :: Int -> Int -> Bool -> Int
execJmz end pc eqz = if pc == end && eqz == True then end 
                else pc

execCpe :: Int -> Int -> Int
execCpe end acc = if acc == end then  0
                else 1

execAdd :: Int -> Int -> Int 
execAdd end acc = if (acc+end) < 127 && (acc+end) > (-128)  then (acc+end)
                 else (-128)

execSub :: Int -> Int -> Int 
execSub end acc = if (acc-end) < 127 && (acc-end) > (-128)  then (acc-end)
                 else (-128)

execNop :: Int
execNop = 0

execHlt :: String 
execHlt = "Programa encerrado."



setAcc :: (Int->Int->Int)-> Int-> Int-> Int
setAcc f end acc= f end acc   



exec :: Reg -> Bool -> [(Int,Int)] -> [(Int,Int)] 
exec acc eqz (m:ms)  | snd m == 2 = let (mem,acc , _) = execLOD (snd (head(ms))) ([m]++ms,acc,eqz)
                            in mem
        
--[(0,2), (1,240)]
-- (m:ms)
-- m = (0,2)
-- ms = [(1,240)]

main  :: IO()

--acc = posição 100

main = do
    let mem = [(0,2),(1,240),(2,14),(3,241),(4,4),(5,251),(6,20),(7,18),(240,10),(241,1),(251,0)]
    let acc = 0
    let eqz = True
    let newMem = exec acc eqz mem
    print acc
    print newMem









