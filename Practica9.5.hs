-- PD-Práctica 9.5 2019-20
-- Tipos: definición y uso de tipos (exámenes antiguos)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Ejercicios 1 y 2. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P1-P5.pdf
-- ---------------------------------------------------------------------


type PilaDeDiscos = [Int]

type  Varilla = String

data TorreDeHanoi = T PilaDeDiscos PilaDeDiscos PilaDeDiscos


moverDisco :: TorreDeHanoi -> Varilla -> Varilla -> TorreDeHanoi

moverDisco (T pi pc pd) "I" "D" = T (tail pi) pc ((head pi):pd)

moverDisco (T pi pc pd) "D" "I" = T ((head pd):pi) pc (tail pd)

moverDisco (T pi pc pd) "C" "D" = T pi (tail pc) ((head pc):pd)



type Termino a = (a, a)

data Polinomio a = PolCero | Pol (Termino a) (Polinomio a)
    deriving Show



grado :: Polinomio Int -> Int

grado PolCero = 0

grado (Pol (_, g) p) = max g (grado p)


-- ---------------------------------------------------------------------
-- Ejercicios 3 y 4. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P2-P6.pdf
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicios 5 y 6. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P3-P7.pdf
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicios 7 y 8. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P4-P8.pdf
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicios 9 y 10. Realice los ejercicios 6 y 7 del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_final/examen.pdf
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 11 y 12. Realice los ejercicios 6 y 7 del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Septiembre/examen.pdf
-- ---------------------------------------------------------------------