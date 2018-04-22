import Text.Show.Functions

--Punto 3.1.1
type Cantidad = [Int]
type Acumulador = Int
type PC = Int
type Etiqueta = String

data Microprocesador = Microprocesador {cantidad::Cantidad,acumuladorA::Acumulador,acumuladorB::Acumulador,programCounter::PC, etiqueta::Etiqueta} deriving Show

--Punto 3.1.2
xt8088 = Microprocesador [] 0 0 0 ""

--Punto 3.2.1

--nop nuevo microprocesador = microprocesador {programCounter = nuevo}

nop (Microprocesador cantidad acumuladorA acumuladorB programCounter etiqueta) = Microprocesador cantidad acumuladorA acumuladorB (programCounter + 1) etiqueta

--Punto 3.2.2
nopTresVeces micro = (nop.nop.nop) micro
--Acá interviene el concepto de Orden superior, cada funcion Nop espera como paramentro el valor de PC de la Nop anterior para sumarle uno a ese valor

--Punto 3.3.1
lodv val (Microprocesador cantidad acumuladorA acumuladorB programCounter etiqueta) = Microprocesador cantidad (acumuladorA + val) acumuladorB (programCounter + 1) etiqueta
swap (Microprocesador cantidad acumuladorA acumuladorB programCounter etiqueta) = Microprocesador cantidad acumuladorB acumuladorA (programCounter + 1) etiqueta
add (Microprocesador cantidad acumuladorA acumuladorB programCounter etiqueta) = Microprocesador cantidad (acumuladorA + acumuladorB) (acumuladorB - acumuladorB) (programCounter + 1) etiqueta

--Punto 3.3.2
sumaDiezMasVeintidos val1 val2 micro = (add.lodv val2.swap.lodv val1) micro

--3.4.1
divide (Microprocesador cantidad acumuladorA acumuladorB programCounter etiqueta) = Microprocesador cantidad (div acumuladorA acumuladorB) (acumuladorB - acumuladorB) (programCounter + 1) etiqueta 

str addr val (Microprocesador cantidad acumuladorA acumuladorB programCounter etiqueta) = Microprocesador (take addr cantidad ++ [val] ++ drop (addr + 1) cantidad) acumuladorA acumuladorB (programCounter + 1) etiqueta

--