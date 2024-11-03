--Definição dos tipos



type Memory = [(Int,Int)]

type PC = Int

type Reg = Int

type Flag = Bool

data CPU = CPU {
    instructionReg :: InstructionReg,
    acc :: Reg
}

type Registers = [Reg]

data InstructionReg = InstructionReg{
    code :: Int,
    address :: Int
}

data InstructionReg = InstructionReg{
    code = 0,
    address = 0
}

--data Instruction 

--Começo do computador

createMem :: Memory 

createMem = [(x,y) | x <- [0], y <- [0..255]]


createEqz :: Flag

createEqz = True

createACC :: Reg

createACC = 0

createPC :: PC
 
createPC = 0

createCPU :: CPU

createCPU = CPU 0









