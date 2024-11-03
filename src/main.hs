--Definição dos tipos



type Memory = [(Int,Int)]

type PC = Int

type MemoryPos = Int

data Reg = Reg { 
    name :: String,
    value :: Int
} deriving Show

type Flag = Bool


data CPU = CPU {
    instructionReg :: IReg,
    pc :: PC
}

type Registers = [Reg]

-- Instructions register
type IReg = Reg

data ULA = ULA{
    eqz :: Flag,
    acc :: Reg
} deriving Show

data Computer = Computer{
    cpu :: CPU,
    mem :: Memory,
    ula :: ULA,
    registers :: Registers
}

--data Instruction 

--Começo do computador



main  :: IO()

main = do
    let mem = [(x,y) | x <- [0], y <- [0..255]]
    let ula = ULA{eqz = True, acc = Reg{name = "acc", value = 0}}


    print mem
    print ula







