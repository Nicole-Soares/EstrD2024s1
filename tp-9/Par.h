#include <iostream>
using namespace std;


//la representacion
struct Par {
int x;
int y;
};

//la interfaz
Par consPar(int x, int y);
int fst(Par p);
int snd(Par p);
int maxDelPar(Par p);
Par swap(Par p);
Par divisionYResto(int n, int m);
