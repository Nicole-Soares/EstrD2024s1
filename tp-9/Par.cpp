#include <iostream>
using namespace std;


#include "Par.h"

//implementacion
Par consPar(int x, int y){
    Par p;
    p.x = x;
    p.y = y;
    return(p);
}

// Propósito: devuelve la primera componente
int fst(Par p){
    return(p.x);
}

// Propósito: devuelve la segunda componente
int snd(Par p){
    return(p.y);
}

// Propósito: devuelve la mayor componente
int maxDelPar(Par p){
    int primero = fst (p);
    int segundo = snd (p);
    if(primero > segundo){
        return primero;
    }
    return segundo;
}

// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p){
    
    return consPar (p.y, p.x);
}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m){
    return consPar (n, m);
}


