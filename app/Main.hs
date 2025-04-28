--module Main where

type Poder = Persona -> Persona

data Persona = UnaPersona {
    nombre :: String,
    poderBasico :: Poder,
    superPoder :: Poder,
    tieneSuperPoderActivo :: Bool,
    cantidadDeVida :: Int
}
 

obtenerCantidadDeVida :: Persona -> Int
obtenerCantidadDeVida unaPersona = cantidadDeVida unaPersona
bolaEspinosa :: Poder
bolaEspinosa unaPersona 
  | obtenerCantidadDeVida unaPersona > 1000 = unaPersona {cantidadDeVida = cantidadDeVida unaPersona - 1000}
  | obtenerCantidadDeVida unaPersona < 1000 = unaPersona {cantidadDeVida = 0}

lluviaDeTuercas :: String -> Poder
lluviaDeTuercas "Sanadora" unaPersona = unaPersona {cantidadDeVida = cantidadDeVida unaPersona + 800}
lluviaDeTuercas "Dañina" unaPersona = unaPersona {cantidadDeVida = div (cantidadDeVida unaPersona) 2 }
lluviaDeTuercas _ unaPersona = unaPersona {cantidadDeVida = cantidadDeVida unaPersona}

granadaDeEspinas :: Int -> Poder
granadaDeEspinas unRadio unaPersona
  |unRadio > 3 && (obtenerCantidadDeVida unaPersona) < 800 = unaPersona {nombre = nombre unaPersona ++ "Espina estuvo aqui",  tieneSuperPoderActivo = False, cantidadDeVida = 0 }
  |unRadio > 3 = unaPersona {nombre = nombre unaPersona ++ "Espina estuvo aqui"}
  |otherwise = bolaEspinosa unaPersona

torretaCurativa :: Poder 
torretaCurativa unaPersona = unaPersona {tieneSuperPoderActivo = True, cantidadDeVida = cantidadDeVida unaPersona*2 }

{-
pamela = UnaPersona  {
    nombre = "Pamela" ,
    poderBasico = lluviaDeTuercas "Sanadora",
    superPoder = torretaCurativa,
    tieneSuperPoderActivo = False,
    cantidadDeVida = 9600
} 

espina = UnaPersona { 
    nombre = "Espina" ,
    poderBasico = bolaEspinosa,
    superPoder = granadaDeEspinas 5,
    tieneSuperPoderActivo = True,
    cantidadDeVida = 4800
} 
-}

