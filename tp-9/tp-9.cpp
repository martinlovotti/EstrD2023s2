#include <iostream>
#include <algorithm>
using namespace std;

struct Par {
int x;
int y;
};
// Propósito: construye un par
Par consPar(int x, int y){
    Par p;
    p.x = x;
    p.y = y;
    return p;
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
    return max (p.x,p.y);
}
// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p){
    return (consPar(p.y, p.x));
}
// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m){
    int x = n/m;
    int y = n-m;
    return(consPar(x,y));
}

void showPar(Par p){
    cout << "Par(";
    cout << "fst <- " << p.x;
    cout << " scn <- " << p.y;
    cout << ")" << endl;
}


int main() {
  Par miPar = consPar(13,22);
  showPar(miPar);
}