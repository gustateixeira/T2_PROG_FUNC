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
        if biggerthan251 /= []
            then print $ snd $ head biggerthan251
        else putStr "" 

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
    
    {-
    1) Resp = A + B - 2 
    enderecos:
    240 - A
    241 - B
    242 - 2
    251 - resultado final
    
    Assembler:
    0 LOD 240
    2 ADD 241
    4 SUB 242
    6 STO 251
    8 HLT NOP

    Resultado:
    = 12 + 3 -2 = 13

    lista: [(0,2),(1,240),(2,14),(3,241),(4,16),(5,242),(6,4),(7,251),(8,20),(9,18),(240,12),(241,3),(242,2),(251,0)] = 12 + 3 -2


    2) 
    
    2) Resp = A * B

    enderecos:
    240-A
    241-B
    242-p/subtrair 1
    243-p/resultado parcial (inicia em 0)
    244-recursao
    251-resultado final

    Assembler:
    0 LOD 241 --inicia recursao com valor B.
    2 STO 244
    4 JMZ 20 --se for 0 vai pro fim. resultado parcial é 0 (X * 0)
    6 LOD 243
    8 ADD 240 --soma 1 vez A
    10 STO 243
    12 LOD 244 --diminui o contador
    14 SUB 242
    16 STO 244
    18 JMP 4 --recursao
    20 LOD 243 --coloca resultado parcial no final
    22 STO 251
    24 HLT NOP


    lista (3 * 4):
    [(0,2),(1,241),(2,4),(3,244),(4,8),(5,20),(6,2),(7,243),(8,14),(9,240),(10,4),(11,243),(12,2),(13,244),(14,16),(15,242),(16,4),(17,244),(18,6),(19,4),(20,2),(21,243),(22,4),(23,251),(24,20),(25,18),(240,3),(241,4),(242,1),(243,0),(244,0),(251,0)]

    Resultado : 12

    3) A = 0; Resp = 1; while(A < 5) { A = A + 1; Resp = Resp + 2; }

    enderecos:
    240-A (inicia em 0)
    241-Resp (inicia em 1)
    242-1
    243-2
    244-5
    251-resultado final

    lista:
    [(0,2),(1,240),(2,10),(3,244),(4,8),(5,20),(6,2),(7,240),(8,14),(9,242),(10,4),(11,240),(12,2),(13,241),(14,14),(15,243),(16,4),(17,241),(18,6),(19,0),(20,2),(21,241),(22,4),(23,251),(24,20),(25,18),(240,0),(241,1),(242,1),(243,2),(244,5),(251,0)]

    Assembler:
    0 LOD 240
    2 CPE 244 –compara 5 com A
    4 JMZ 20
    6 LOD 240 –a = a + 1
    8 ADD 242
    10 STO 240
    12 LOD 241 –resp = resp + 2
    14 ADD 243
    16 STO 241
    18 JMP 0
    20 LOD 241 –fim da recursao
    22 STO 251
    24 HLT NOP

    Resultado: 11

    -}
    
    
    let commands = "Selecione qual exemplo:\n-> Digite {1} para -> 12 + 3 - 2 \n-> Digite {2} para 3 * 4\n-> Digite {3} para  A = 0; Resp = 1; while(A < 5) { A = A + 1; Resp = Resp + 2; }\n"
    putStr commands
    input <- readLn :: IO Int

    prog <- if input == 1 then 
                readFromFile "input/input1.txt" 
            else if input == 2  then 
                readFromFile "input/input2.txt"
            else if input == 3 then 
                readFromFile "input/input3.txt" 
            else do
                putStrLn "Entrada inválida"
                return []
    
    if prog == [] then return ()
        else do
        let cpu = createCpu
        let cpuWithMemory = loadMemory prog cpu
        let cpuFinal = runCpu cpuWithMemory
        _ <- cpuFinal
        return ()








