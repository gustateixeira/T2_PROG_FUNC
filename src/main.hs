--Definição dos tipos

import System.IO

type Memory = [(Int,Int)]

type Reg = Int
data ULA = ULA{
    acc :: Reg,
    eqz :: Bool 
} deriving Show

data CPU = CPU{
    ula :: ULA,
    pc :: Reg,
    memory :: Memory,
    ir :: (Reg,Reg)
}deriving Show



{-Escrita da memória-}
writeMem :: Memory -> Int -> Reg -> Memory
writeMem (m:ms) end acc | end == fst m = [(fst m, acc)] ++ ms
                        | otherwise = m : writeMem ms end acc

{-Leitura da memória-}
readMem :: Memory -> Int -> Int
readMem (m:ms) end  
    | end == fst m = snd m
    | otherwise = readMem ms end    


{-Carrega o conteúdo do endereço de memória <end> no
registrador acumulador (ACC).-}

execLod :: CPU ->  CPU
execLod cpu | acc (ula cpu) /= 0 = cpu {ula = (ula cpu){acc = value, eqz = False} , pc = pc cpu + 2}
            | otherwise = cpu {ula = (ula cpu){acc = value, eqz = True} , pc = pc cpu + 2}
        where value = readMem (memory cpu) (snd(ir cpu))

{-
Armazena o conteúdo do registrador acumulador (ACC) no
endereço de memória <end>
-}
execSto :: CPU ->  CPU
execSto cpu = cpu{ memory = newmemory, pc = pc cpu + 2}
        where newmemory = writeMem (memory cpu) (snd(ir cpu)) (acc(ula cpu))

{-
Desvio incondicional: carrega no contador de instruções (PC) o valor
<end> forçando com que a próxima instrução a ser executada
seja a que se encontra no endereço de memória <end>
-}

execJmp :: CPU -> CPU
execJmp cpu = cpu{pc = end}
            where end = (snd(ir cpu))


{-
Desvio condicional: funcionamento análogo ao da instrução JMP
com a diferença que a carga do contador de instruções só ocorre
se o valor do acumulador for igual a zero (de acordo com a flag
EQZ)

execJmz end pc eqz = if pc == end && eqz == True then end 
                else pc

-}

execJmz :: CPU -> CPU
execJmz cpu = if eqz(ula cpu) == True then cpu{pc = end} else cpu{pc = pc cpu + 2}
                where end = (snd(ir cpu))


{-
Se o conteúdo do endereço <end> for igual ao acumulador,
coloca 0 no acumulador, caso contrário coloca 1

end acc = if acc == end then  0
                else 1

-}

execCpe :: CPU -> CPU
execCpe cpu = if acc(ula cpu) == end then cpu{ pc = pc cpu + 2, ula = (ula cpu) {acc = 0}} 
            else cpu {pc = pc cpu + 2,ula = (ula cpu) {acc = 1}} 
            where end = readMem (memory cpu) (snd(ir cpu))

{-
Adiciona o conteúdo do endereço de memória <end> ao
conteúdo armazenado no acumulador (ACC) e armazena a
resposta no próprio acumulador
-}

execAdd :: CPU -> CPU
execAdd cpu   | ((acc'+end) < 127 && (acc'+end) > (-128))= cpu{ula = (ula cpu){acc = (acc(ula cpu)) + end}, pc = pc cpu + 2}
              |otherwise =  cpu{ula = (ula cpu){acc = -128}}
              where acc' = acc(ula cpu)
                    end = readMem (memory cpu) (snd(ir cpu))

{-
Subtrai o conteúdo do endereço de memória <end> do conteúdo
do acumulador (ACC) e armazena a resposta no próprio
acumulador
-}

execSub :: CPU -> CPU
execSub cpu | ( (acc'-end) < 127 && (acc'-end) > (-128)) = cpu{ula = (ula cpu){acc = (acc (ula cpu)) - end}, pc = pc cpu + 2}
            |otherwise =  cpu{ula = (ula cpu){acc = -128}}
              where acc' = acc(ula cpu)
                    end = readMem (memory cpu) (snd(ir cpu)) 

{-
Não executa ação nenhuma (No OPeration)
-}

execNop :: CPU -> CPU
execNop cpu =  cpu{pc = pc cpu + 2}

{-
Encerra o ciclo de execução do processador (HaLT)
-}
execHlt :: CPU -> CPU 
execHlt cpu = cpu{pc = -1}


createMemory :: Memory
createMemory = zip [0..255] (repeat 0)

createUla :: ULA
createUla = ULA{
    acc = 0,
    eqz = True
}


createCpu :: CPU
createCpu = CPU{
    ula = createUla,
    pc = 0,
    memory = createMemory,
    ir = (0 , 0)
}

finalMemory :: CPU -> Memory
finalMemory cpu = memory cpu

loadMemory :: Memory -> CPU -> CPU
loadMemory [] cpu = cpu 
loadMemory mem cpu = cpu{memory = mem}


decode :: CPU -> CPU
decode cpu = case fst (ir cpu) of
    2  -> execLod cpu
    4  -> execSto cpu
    6  -> execJmp cpu
    8  -> execJmz cpu
    10 -> execCpe cpu
    14 -> execAdd cpu
    16 -> execSub cpu
    18 -> execNop cpu
    20 -> execHlt cpu
    _ -> cpu

fetch :: CPU ->  (Reg,Reg)
fetch cpu = (snd(memory'!!pc'),snd(memory'!!(pc'+1)))
        where memory' = memory cpu
              pc' = pc cpu

setFlag :: CPU -> CPU
setFlag cpu = if acc(ula cpu) /= 0 then cpu{ula = (ula cpu){eqz = False}}
              else cpu{ula = (ula cpu){eqz = True}}

filteredMemory :: Memory -> Memory
filteredMemory [] = []
filteredMemory m = filter(\(add,v) -> add >= 251 && v> 0) m

runCpu :: CPU -> IO CPU 
runCpu cpu = do

        let cpuWithIr = cpu { ir = fetch cpu }
            opcode = fst(ir cpuWithIr)            
            settedCpu = setFlag cpuWithIr
            nextCpu = decode settedCpu                 
            biggerthan251 = filteredMemory (memory nextCpu)
        print biggerthan251
        if opcode == 20
            then return cpuWithIr
        else runCpu nextCpu

        


--transforma a lista numa tupla
-- let[addr,val] significa que pega a linha e transforma ela numa lista de [endereco, valor]
-- words(line) pega a linha e transforma ela numa lista das palavras
-- map read(words line) pega a lista de palavras e transforma em uma lista de inteiros
-- in (addr,val) coloca essa lista numa tupla
parseLine :: String -> (Int,Int)
parseLine line = let [addr, val] = map read(words line) in (addr,val)


readFromFile :: FilePath -> IO Memory
readFromFile path = do
    content <- readFile path
    return $ map parseLine (lines content)

main  :: IO()

--acc = posição 100

main = do
    

    prog1 <- readFromFile "input/input1.txt" 
    prog2 <- readFromFile "input/input2.txt"
    prog3 <- readFromFile "input/input3.txt"
    let cpu = createCpu 
    let cpuWithMemory = loadMemory prog3 cpu
    let cpuFinal = runCpu cpuWithMemory
    _ <- cpuFinal
    return ()








