--Definição dos tipos









type Flag = Bool

data CPU = CPU {
    instructionReg :: Int,
    pc :: Int,
    rdm :: Int,
    rem' :: Int
}deriving Show


data ULA = ULA{
    eqz :: Bool,
    acc :: Int
} deriving Show

data Computer = Computer{
    cpu :: CPU,
    mem :: [(Int,Int)],
    ula :: ULA
}deriving Show

--Criação da instrução 




--Começo do computador




createComputer :: CPU-> [(Int,Int)]-> ULA-> Computer
createComputer c m u = Computer{cpu = c, mem = m, ula = u}

createCpu :: CPU
createCpu = CPU{instructionReg = 0, 
            pc = 0,
            rdm = 0, 
            rem' = 0}


-- Criação das instruções

--LOD
execLOD :: Int -> ([(Int,Int)], Int, Bool) -> ([(Int,Int)], Int, Bool)
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
execSto :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
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
execHlt = "Programa encerrado.  "



setAcc :: (Int->Int->Int)-> Int-> Int-> Int
setAcc f end acc= f end acc   



--exec :: [(Int,Int)] -> [(Int,Int)]


main  :: IO()

main = do
    let mem = [(x,y) | x <- [0..255], y <- [0]]
    let ula = ULA{eqz = True, acc = 0}
    let instruction_register =  0
    let cpu = createCpu
    let computer = createComputer cpu mem ula
    let afterWriting = execSto mem 3 10
    let exc = execLOD 2 (mem, (acc ula), (eqz ula))
    let excJmp = execJmp 2 (pc cpu)
    print afterWriting
    print (readMem mem 2)
    let execjmz = execJmz 

    print excJmp








