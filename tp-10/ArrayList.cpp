#include <iostream>
#include "ArrayList.h"
using namespace std;

struct ArrayListSt{
    int cantidad;
    int* elementos;
    int capacidad;
};

ArrayList newArrayList(){
    ArrayListSt* a = new ArrayListSt;
    a-> cantidad = 0;
    a-> capacidad = 16;
    a-> elementos = new int[a -> capacidad];
    return a;
};

ArrayList newArrayListWith(int cap){
    ArrayListSt* new_array =  new ArrayListSt;
    new_array-> cantidad = 0;
    new_array-> capacidad = cap;
    new_array-> elementos = new int[new_array-> capacidad];
    return new_array;
}; 

int lenghtAl(ArrayList xs){
    return xs-> cantidad;
};

int get ( int i , ArrayList xs){
   return xs->elementos[i];
};

void set(int i, int x, ArrayList xs){
    xs->elementos[i]=x;
};

void resize (int capacidad, ArrayList xs){
    int* eN = new int[capacidad];

    for(int i=0; i < min(capacidad, xs->cantidad) ;i++){
        eN[i] = xs->elementos[i];
    }

    delete (xs->elementos);
    xs->elementos=eN;
    xs->capacidad=capacidad;
    xs->cantidad=min(capacidad, xs->cantidad);
};

void add(int x, ArrayList xs){
    if(xs->cantidad == xs->capacidad){
        duplicarCap(xs);
    }
    xs->elementos[xs->cantidad]=x;
    xs->cantidad++;   
};

void duplicarCap(ArrayList xs){
    int* eN = new int[xs->capacidad*2];
    for (int i=0; i< xs->cantidad; i++){
        eN[i] = xs->elementos[i];
    }
    delete (xs->elementos);
    xs->elementos=eN;
    xs->capacidad *=2;  
};

void remove (ArrayList xs){
    xs->cantidad--;
}

/**
int main() {
    ArrayList a = newArrayList();
    add(3, a);
    cout << " cantidad en array <- " << a->cantidad << endl;
    cout << " elementos en array <- " << a->elementos[0]<< endl;
    cout << " capacidad en array <- " << a->capacidad<< endl;
}*/
