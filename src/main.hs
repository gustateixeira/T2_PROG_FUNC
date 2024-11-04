--Definição dos tipos



type Memory = [(Int,Int)] 

type MemoryPos = Int

data EightBitsReg = EightBitsReg {
    value :: Int
} deriving Show

type SixTeenBitsReg = Int

type Flag = Bool


data CPU = CPU {
    instructionReg :: SixTeenBitsReg,
    pc :: EightBitsReg,
    rdm :: EightBitsReg,
    rem' :: EightBitsReg,
    instructionsSet :: Instructions
}deriving Show


data ULA = ULA{
    eqz :: Flag,
    acc :: EightBitsReg
} deriving Show

data Computer = Computer{
    cpu :: CPU,
    mem :: Memory,
    ula :: ULA
}deriving Show

--Criação da instrução 

data Instruction = Instruction{
    code :: Int,
    name :: String 
}deriving Show
-- Instruções são uma lista de instrução
type Instructions = [Instruction]

--Começo do computador


setInstructions :: Instructions
setInstructions = [Instruction{code = 2, name = "LOD"},
                   Instruction{code = 4, name = "STO"},
                   Instruction{code = 6, name = "JMP"},
                   Instruction{code = 8, name = "JMZ"},
                   Instruction{code = 10, name = "CPE"},
                   Instruction{code = 14, name = "ADD"},
                   Instruction{code = 16, name = "SUB"},
                   Instruction{code = 18, name = "NOP"},
                   Instruction{code = 20, name = "HLT"}]


createComputer :: CPU-> Memory-> ULA-> Computer
createComputer c m u = Computer{cpu = c, mem = m, ula = u}

createCpu :: CPU
createCpu = CPU{instructionReg = 0, pc = EightBitsReg {value =0}, rdm = EightBitsReg {value = 0}, rem' = EightBitsReg{value = 0}, instructionsSet = setInstructions}



main  :: IO()

main = do
    let mem = [(x,y) | x <- [0], y <- [0..255]]
    let ula = ULA{eqz = True, acc = EightBitsReg{ value = 0}}
    let instruction_register =  0

    let cpu = createCpu

    let computer = createComputer cpu mem ula

    print computer







