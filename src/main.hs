--Definição dos tipos



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


exec :: CPU -> CPU
exec cpu = case fst (ir cpu) of
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

busca :: CPU ->  (Reg,Reg)
busca cpu = (snd(memory'!!pc'),snd(memory'!!(pc'+1)))
        where memory' = memory cpu
              pc' = pc cpu

setFlag :: CPU -> CPU
setFlag cpu = if acc(ula cpu) /= 0 then cpu{ula = (ula cpu){eqz = False}}
              else cpu{ula = (ula cpu){eqz = True}}

runCpu :: CPU -> CPU 
runCpu cpu | opcode == 20 = cpu{ir = (20, snd(memory cpu!!((pc cpu) + 1)))}
           | otherwise = runCpu nextCpu
        where 
            cpuWithIr = cpu { ir = busca cpu }
            settedCpu = setFlag cpuWithIr
            opcode = fst(ir cpuWithIr)
            nextCpu = exec settedCpu

        


main  :: IO()

--acc = posição 100

main = do
    

    let prog1 = [(0,2),(1,240),(2,14),(3,241),(4,4),(5,251),(6,20),(7,18),(240,0),(241,1),(251,0)]
    let c = createCpu
    let prog2 = [(0,2),(1,241),(2,4),(3,244),(4,8),(5,20),(6,2),(7,243),(8,14),(9,240),(10,4),(11,243),(12,2),(13,244),(14,16),(15,242),(16,4),(17,244),(18,6),(19,4),(20,2),(21,243),(22,4),(23,251),(24,20),(25,18),(240,3),(241,4),(242,1),(243,0),(244,0),(251,0)]
    

    let prog3 = [(0,2),(1,240),(2,10),(3,244),(4,8),(5,20),(6,2),(7,240),(8,14),(9,242),(10,4),(11,240),(12,2),(13,241),(14,14),(15,243),(16,4),(17,241),(18,6),(19,0),(20,2),(21,241),(22,4),(23,251),(24,20),(25,18),(240,0),(241,1),(242,1),(243,2),(244,5),(251,0)]

    let load = loadMemory prog3 c 
    print load
    let exc = runCpu load
    
    print exc








