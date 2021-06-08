module Library where
import PdePreludat

--Kalinin Alexander

--Modelado de Guantelete

data Guantelete= UnGuantelete {
    material :: String
,   gemas :: [Gema]    
} deriving (Show, Eq)

--Modelado de Personajes

data Personaje= UnPersonaje {
    edad :: Number
,   energia :: Number
,   habilidades :: [String]
,   nombre :: String
,   planeta :: String 
} deriving (Show, Eq)

--Modelado de aUniverso
data Universoo = UnaUniverso {
    poblacion :: [Personaje]
} deriving (Show,Eq)
 
--Dos opciones  
type Universo = [Personaje]

--Personajes ejemplos---------------------

ironMan= UnPersonaje {
    edad = 41
,   energia = 30
,   habilidades = ["volar","inteligencia", "liderazgo"]
,   nombre = "tony Stark"
,   planeta ="Tierra" 
} 

viudaNegra= UnPersonaje {
    edad = 35
,   energia = 20
,   habilidades = ["pelear", "manipular"]
,   nombre = "Natasha Romanoff"
,   planeta ="Tierra" 
} 

drStrange= UnPersonaje {
    edad = 47
,   energia = 40
,   habilidades = ["usar hechizos"]
,   nombre = "Stephen Vincent"
,   planeta ="Tierra" 
} 
------------------------------------------

--Punto 1)

guanteleteCompleto = UnGuantelete{
  material = "uru"
, gemas = [gemaMente 10 , gemaAlma "volar", gemaEspacio "tierra", gemaPoder, gemaTiempo, gemaLoca (gemaTiempo)]
}



chasquearGuantelete :: Guantelete->Universo->Universo
chasquearGuantelete aGuantelete aUniverso | ((6==).length.gemas) aGuantelete && (("uru"==) .material) aGuantelete = eliminarPoblacion aUniverso
                                          | otherwise = aUniverso


eliminarPoblacion :: Universo->Universo
eliminarPoblacion aUniverso =  drop (cantidadAEliminar  aUniverso) (reverse aUniverso)

cantidadAEliminar :: Universo->Number
cantidadAEliminar  = ceiling .(/2) . length 

--cantidadAEliminar' = ceiling .(/2) . length . poblacion


--Punto 2)

--a)
univerAptoParaPendex :: Universo->Bool
univerAptoParaPendex  = any (menorA 45 ) 

menorA :: Number->Personaje->Bool
menorA n = (<n) . edad


--b) Filter y any funciones de orden superior
type Energia = Number

energiaTotal :: Universo->Energia
energiaTotal  = sum . extraerEnergiaPersonsajes
 

extraerEnergiaPersonsajes :: Universo->[Energia]
extraerEnergiaPersonsajes aUniverso = map energia (personasConMasDeUnaHabilidad aUniverso)


personasConMasDeUnaHabilidad :: Universo->Universo
personasConMasDeUnaHabilidad = filter (tenerMasHabilidad 1 )  

tenerMasHabilidad :: Number->Personaje->Bool
tenerMasHabilidad n = (>1) . length. habilidades


---------------------Segunda Parte------------------------------

-------
type Valor = Number

gemaMente :: Valor->Gema
gemaMente valor   = debilitarEnergia valor 

debilitarEnergia :: Valor->Personaje->Personaje
debilitarEnergia valor aPersonaje = aPersonaje {energia= (energia aPersonaje)-valor}
-------
type Habilidad = String

gemaAlma :: Habilidad->Gema
gemaAlma habilidad  =  quitarHabilidadParticular habilidad . debilitarEnergia 10

quitarHabilidadParticular :: Habilidad->Personaje->Personaje
quitarHabilidadParticular habilidad aPersonaje = aPersonaje {habilidades= eliminarHabilidadParticular habilidad (habilidades aPersonaje)}


eliminarHabilidadParticular :: Habilidad->[Habilidad]->[Habilidad]
eliminarHabilidadParticular habilidad  = filter (habilidadDistinta habilidad) 

habilidadDistinta :: Habilidad->Habilidad->Bool
habilidadDistinta habilidad  = (/=habilidad)
----------
type Planeta = String

gemaEspacio :: Planeta->Gema
gemaEspacio planeta  = transportarPlaneta planeta. debilitarEnergia 20

transportarPlaneta :: Planeta->Personaje->Personaje
transportarPlaneta planeta aPersonaje = aPersonaje {planeta=planeta } 

----------

gemaPoder :: Gema
gemaPoder aPersonaje = quitarHabilidad (debilitarEnergia (energia aPersonaje) aPersonaje) 

quitarHabilidad :: Gema
quitarHabilidad aPersonaje |(length.habilidades) aPersonaje < 3 = aPersonaje {habilidades = []}
                           | otherwise = aPersonaje

-----------
type Edad = Number
gemaTiempo :: Gema
gemaTiempo aPersonaje = aPersonaje{edad= reducirEdad (edad aPersonaje)}

reducirEdad :: Edad->Edad
reducirEdad  =  max 18 . floor .(/2)
---------------
type Gema = Personaje->Personaje

gemaLoca :: Gema->Gema
gemaLoca gema = gema . gema
----------
--Punto3)
--Foldear funciones 
utilizarPoderGema :: Personaje->Gema->Personaje
utilizarPoderGema aPersonaje gema  = gema aPersonaje

utilizarGuantelete :: Guantelete->Personaje->Personaje
utilizarGuantelete guantelete aPersonaje = foldl utilizarPoderGema aPersonaje (gemas guantelete)  

---Guante de ejemplo
guantelete2 = UnGuantelete{
  material = "uru"
, gemas = [gemaMente 15 , gemaAlma "volar", gemaEspacio "marte"]
}

--utilizarGuantelete guantelete2 ironMan => 
-- UnPersonaje {edad = 41, energia = -15, habilidades = ["inteligencia","liderazgo"], nombre = "tony Stark", planeta = "marte"}


-- Punto 4)

guanteleteGoma = UnGuantelete{
  material = "uru"
, gemas = [gemaTiempo , gemaAlma "usar Mjolnir", gemaLoca (gemaAlma "programacion Haskell")]
}

thor= UnPersonaje {
    edad = 50000
,   energia = 1250
,   habilidades = ["volar","fuerza", "usar Mjolnir", "programacion Haskell"]
,   nombre = "Thor Odinson"
,   planeta ="Asgard" 
} 
-- utilizarGuantelete guanteleteGoma thor =>
-- UnPersonaje {edad = 25000, energia = 1220, habilidades = ["volar","fuerza"], nombre = "Thor Odinson", planeta = "Asgard"}


--PUNTO 5

utilizar :: [Gema]-> Personaje->[Personaje]
utilizar gemas aPersonaje = map (utilizarPoderGema aPersonaje) gemas

-- Sin recursion no vas a poder obtener el personaje final con los cambios ya que no podes hacer que se acoplen los efectos de los personajes
-- es decir cuando sale el personaje modificado no puede entrar a otra gema para producirse un nuevo cambio (x2 en este caso)


--Punto 6

--gemaMasPoderosa :: Guantelete->Personaje->Gema
--gemaMasPoderosa (primerGema : gemas) aPersonaje | (energia.primerGema )aPersonaje > energia aPersonaje=  primerGema
                                           --     | otherwise = primerGema


gemasMasPoderosa :: Personaje->[Gema]->Gema
gemasMasPoderosa aPersonaje gemas = maximoSegun ( energia. (utilizarPoderGema aPersonaje)) gemas

--pasas dose gemas a la funcion menorSegun para ahi poder realziar la comparacion, se devuelve la q disminuye mas energia para luego comparara con la siguiente gema
maximoSegun :: Ord b => (a->b)->[a]->a
maximoSegun f = foldl1 (mayorSegun f) 


--Te vas quedando con la gema que deja con menos energia
menorSegun :: Ord x => (t->x)->(t->t->t)
menorSegun f a b
  | f a < f b = a
  | otherwise = b


-- Punto 7)
--a) NO se puede ya que es una lista infinita y haskell no puede evaluar todas las gemas, no termina mas la evaluacion
-- b) Si se puede ejecutar por el conceto de lazy evaluation, va a tomar los primeros elementos de la lista infinita, descartando los demas