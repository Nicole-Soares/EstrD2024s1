#include <iostream>
#include "Par.h"

using namespace std;


// La interfaz
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


int main() {
    //printN
    printN(5, "n");
    return 0;

}





void ShowPar(Par p) {
    std::cout << "(" << p.x << ", " << p.y << ")" << std::endl;
}

