#include <iostream>
#include "ArrayList.h"
using namespace std;

//Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs){
    int acc = 0;
    for(int i=0; i < lenghtAl(xs); i++){
        acc += get(i,xs);
    }
    return acc;
}

//Incrementa en uno todos los elementos.
void sucesores(ArrayList xs){
    for(int i=0; i < lenghtAl(xs);i++){
        set(i, get(i,xs)+1 , xs);
    }
}

//Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs){
    for(int i=0; i < lenghtAl(xs); i++){
        if(get(i,xs)==x){
            return true;
        }
    }
    return false;
}

//Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs){
    int acc = 0;
    for(int i=0; i < lenghtAl(xs); i++){
        if(get(i,xs)==x){
            acc++;
        }
    }
    return acc;
}

//Crea una nueva lista a partir de la primera y la segunda (en ese orden).
ArrayList append(ArrayList xs, ArrayList ys){
    ArrayList a = newArrayList();
    for (int i = 0; i <lenghtAl(xs) ; i++){
        add(get(i,xs),a);
    }
    for (int i = 0; i <lenghtAl(ys) ; i++){
        add(get(i,ys),a);
    }
    return a;
}



int main(){
    ArrayList a = newArrayList();
    add(3, a);
    add(2, a);
    ArrayList b = newArrayList();
    add(1, b);
    add(2, b);
    ArrayList c = append(a,b);
    cout << "apariciones de 2 en array <- " << apariciones(1,c)<< endl;
    
}
