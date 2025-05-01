module Lib () where

import Text.Show.Functions ()

type Poder = Personaje -> Personaje

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: Poder,
    superPoder :: Poder,
    tieneSuperPoderActivo :: Bool,
    cantidadDeVida :: Int
} 

instance Show Personaje where
  show (UnPersonaje nombre _ _ superPoderActivo vida) = "Personaje {nombre = "++ show nombre ++", cantidadDeVida="++ show vida ++", tieneSuperPoderActivo = "++ show superPoderActivo ++"}"

bolaEspinosa :: Poder
bolaEspinosa unPersonaje
  | cantidadDeVida unPersonaje > 1000 = unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje - 1000}
  | cantidadDeVida unPersonaje < 1000 = unPersonaje {cantidadDeVida = 0}

lluviaDeTuercas :: String -> Poder
lluviaDeTuercas "Sanadora" unPersonaje = unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje + 800}
lluviaDeTuercas "Dañina" unPersonaje = unPersonaje {cantidadDeVida = div (cantidadDeVida unPersonaje) 2 }
lluviaDeTuercas _ unPersonaje = unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje}

granadaDeEspinas :: Int -> Poder
granadaDeEspinas unRadio unPersonaje
  | unRadio > 3 && (cantidadDeVida unPersonaje) < 800 = unPersonaje {nombre = nombre unPersonaje ++ "Espina estuvo aqui",  tieneSuperPoderActivo = False, cantidadDeVida = 0 }
  | unRadio > 3 = unPersonaje {nombre = nombre unPersonaje ++ " Espina estuvo aqui"}
  | otherwise = bolaEspinosa unPersonaje

torretaCurativa :: Poder 
torretaCurativa unPersonaje = unPersonaje {tieneSuperPoderActivo = True, cantidadDeVida = cantidadDeVida unPersonaje *2 }

atacarConElPoderEspecial :: Personaje -> Poder
atacarConElPoderEspecial unPersonaje otroPersonaje
  | tieneSuperPoderActivo unPersonaje = (superPoder unPersonaje.poderBasico unPersonaje) otroPersonaje
  | otherwise = otroPersonaje

pamela :: Personaje
pamela = UnPersonaje  {
    nombre = "Pamela" ,
    poderBasico = lluviaDeTuercas "Sanadora",
    superPoder = torretaCurativa,
    tieneSuperPoderActivo = False,
    cantidadDeVida = 9600
} 

espina :: Personaje
espina = UnPersonaje { 
    nombre = "Espina" ,
    poderBasico = bolaEspinosa,
    superPoder = granadaDeEspinas 5,
    tieneSuperPoderActivo = True,
    cantidadDeVida = 4800
  } 

personajes :: [Personaje]
personajes = [pamela, espina]

quienesEstanEnLasUltimas :: [Personaje]->[String]
quienesEstanEnLasUltimas personajes = nombresDeLosQueEstanEnLasUltimas personajes

estaEnLasUltimas :: Personaje -> Bool
estaEnLasUltimas unPersonaje = ((<800).cantidadDeVida) unPersonaje
nombresDeLosQueEstanEnLasUltimas :: [Personaje]->[String]
nombresDeLosQueEstanEnLasUltimas personajes = (map nombre.filter estaEnLasUltimas) personajes

