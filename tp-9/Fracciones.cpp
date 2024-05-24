#include <iostream>
#include "Fracciones.h"
using namespace std;


//impementacion


// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador){
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return(f);
}
// Propósito: devuelve el numerador
int numerador(Fraccion f){
    return(f.numerador);
}


// Propósito: devuelve el denominador
int denominador(Fraccion f){
    return(f.denominador);
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f){
    return(f.numerador / f.denominador);
}


// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2){
    return(f1 * f2);
}



// Función para calcular el máximo común divisor (MCD)
int mcd(int a, int b) {
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro*/
Fraccion simplificada(Fraccion p) {
    // Calcular el máximo común divisor (MCD) entre el numerador y el denominador
    int divisorComun = mcd(p.numerador, p.denominador);

    // Dividir el numerador y el denominador por el MCD para simplificar la fracción
    p.numerador /= divisorComun;
    p.denominador /= divisorComun;

    return p;
}

// Propósito: devuelve la primera componente
Fraccion sumF(Fraccion f1, Fraccion f2){
Fraccion resultado;
    
    // Si las fracciones tienen el mismo denominador
    if (f1.denominador == f2.denominador) {
        resultado.numerador = f1.numerador + f2.numerador;
        resultado.denominador = f1.denominador;
    } else { // Si las fracciones tienen denominadores diferentes
        // Encontrar el mínimo común múltiplo (mcm) de los denominadores
        int mcm = (f1.denominador * f2.denominador) / mcd(f1.denominador, f2.denominador);
        
        // Convertir las fracciones al mismo denominador
        int factor1 = mcm / f1.denominador;
        int factor2 = mcm / f2.denominador;
        
        f1.numerador *= factor1;
        f1.denominador *= factor1;
        f2.numerador *= factor2;
        f2.denominador *= factor2;
        
        // Sumar los numeradores
        resultado.numerador = f1.numerador + f2.numerador;
        resultado.denominador = mcm;
    }
    
    return resultado;
}