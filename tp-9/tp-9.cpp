#include <iostream>
//#include "Par.h"
using namespace std;
#include "Fracciones.h"


/*// La interfaz
Par consPar(int x, int y);
void ShowPar(Par p);

// Sobrecarga del operador << para imprimir objetos de tipo Par
std::ostream& operator<<(std::ostream& os, const Par& p) {
    os << "(" << p.x << ", " << p.y << ")";
    return os;
}


//Propósito: imprime n veces un string s.
//recursion
void printN(int n, string s){
    if( n > 0){
      cout << s << std::endl; 
       printN( n - 1, s);
    }
}

//iteracion
void printN(int n, string s){
    while (n > 0)
    {
        cout << s << std::endl;
        n = n - 1;
    }
    
}

//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n){
    if(n >= 0){
         cout << n << std::endl; 
         cuentaRegresiva(n - 1);
    }
}

//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
//recursivo
void desdeCeroHastaN(int n){
    if( n >= 0){
        cout << n << std::endl; 
        desdeCeroHastaN (n - 1);
    }
}

//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
// iteracion
void desdeCeroHastaN(int n){

    int m = 0
    while( m <= n){
        cout << m << std::endl;
        m = m + 1;
    }
}

//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
//recursion
int mult(int n, int m){
    
    if( m > 1){
        n = n + n;
        m = m - 1;
        mult(n , m)
    }
    cout << n << std::endl;
}

//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
//iteracion
int mult(int n, int m){
    
    while( m > 1){
        n = n + n;
        m = m - 1;
    }
    cout << n << std::endl;
}*/

/*Propósito: imprime los primeros n char del string s, separados por un salto de línea.
Precondición: el string tiene al menos n char.
//recursion
void primerosN(int n, string s){
    if( n > 0){
        cout << s[0] << std::endl;
        primerosN (n - 1, s.substr(1))
    }
}

//iteracion
void primerosN(int n, string s){
    while(n > 0){
       cout << s[0] << std::endl; 
        n = n-1;
        s = s.substr(1);
    }
}    


//Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s){
   while (not s.empty() && s[0] != c){
    s = s.substr(1);
   };
   return not s.empty();
}*/
/*
//recursion
bool pertenece(char c, string s){


    if(s.empty()){
        return false;
    }
    else {
        if(c != s[0]){
            return pertenece (c, s.substr(1));
        }
        else{
           return true;
        }
    }
 
 
}

*/

//Propósito: devuelve la cantidad de apariciones de un char c en el string s
//iteracion
int apariciones(char c, string s){
    int cantidad = 0;
    while(not s.empty()){
        if(s[0] == c){
            cantidad = cantidad + 1;
        }
       s = s.substr(1);
    }
    return cantidad;
}
/*
//recursion
int apariciones(char c, string s){
    if(s.empty()){
        return 0;
    }
    else{
        if(s[0] == c){
            return 1 + apariciones(c, s.substr(1));
        }
         else{
            return apariciones(c, s.substr(1));
        }
    }

   
}*/

// Sobrecarga del operador << para imprimir una Fraccion
std::ostream& operator<<(std::ostream& os, const Fraccion& f) {
    os << f.numerador << "/" << f.denominador;
    return os;
}

int main() {
    // Declara la variable 'p' y asigna el resultado de pertenece a 'p'

    Fraccion c = consFraccion(2, 3);
    // Imprime el valor de 'p' utilizando cout
    std::cout<< c << std::endl;

    return 0;
}





/*void ShowPar(Par p) {
    std::cout << "(" << p.x << ", " << p.y << ")" << std::endl;
}*/

