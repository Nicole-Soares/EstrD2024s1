#include <iostream>
using namespace std;


//representacion
struct Fraccion {
int numerador;
int denominador;
};

//interfaz
// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador);
/*// Propósito: devuelve el numerador
int numerador(Fraccion f);
// Propósito: devuelve el denominador
int denominador(Fraccion f);
// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f);
// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2);
// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
*/