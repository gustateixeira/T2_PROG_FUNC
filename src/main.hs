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

execLOD :: CPU ->  CPU
execLOD cpu = cpu {ula = (ula cpu){acc = value} , pc = pc cpu + 2}
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
execJmz cpu = if pc cpu == end && eqz(ula cpu) == True then cpu{pc = end} else cpu{pc = pc cpu + 2}
                where end = readMem (memory cpu) (snd(ir cpu))


{-
Se o conteúdo do endereço <end> for igual ao acumulador,
coloca 0 no acumulador, caso contrário coloca 1

end acc = if acc == end then  0
                else 1

-}

execCpe :: CPU -> CPU
execCpe cpu = if acc(ula cpu) == end then cpu{ pc = pc cpu + 2, ula = (ula cpu) {acc = 0}} 
            else cpu {ula = (ula cpu) {acc = 1}} 
            where end = readMem (memory cpu) (snd(ir cpu))

{-
Adiciona o conteúdo do endereço de memória <end> ao
conteúdo armazenado no acumulador (ACC) e armazena a
resposta no próprio acumulador
-}
--execAdd :: CPU -> CPU
--execAdd cpu = if (acc+end) < 127 && (acc+end) > (-128)  then (acc+end)
  --               else (-128)

{-
Subtrai o conteúdo do endereço de memória <end> do conteúdo
do acumulador (ACC) e armazena a resposta no próprio
acumulador
-}

execSub :: Int -> Reg -> Int 
execSub end acc = if (acc-end) < 127 && (acc-end) > (-128)  then (acc-end)
                 else (-128)

{-
Não executa ação nenhuma (No OPeration)
-}

execNop :: Int
execNop = 0

{-
Encerra o ciclo de execução do processador (HaLT)
-}
execHlt :: Int 
execHlt = -1


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
    ir = (0,1)
}

finalMemory :: CPU -> Memory
finalMemory cpu = memory cpu

{-exec :: CPU -> CPU
exec cpu = case fst (ir cpu) of
    2  -> lod cpu
    4  -> sto cpu
    6  -> jmp cpu
    8  -> jmz cpu
    10 -> cpe cpu
    14 -> add cpu
    16 -> sub cpu
    18 -> nop cpu
    20 -> hlt cpu
-}
main  :: IO()

--acc = posição 100

main = do
    let c = createCpu
    print c








