#include <iostream>
using namespace std;

struct ArrayListSt;

typedef ArrayListSt* ArrayList;

ArrayList newArrayList();
ArrayList newArrayListWith(int cap);
int lenghtAl(ArrayList xs);
int get (int i, ArrayList xs);
void set(int i, int x, ArrayList xs);
void resize (int capacidad, ArrayList xs);
void add(int x, ArrayList xs);
void duplicarCap(ArrayList xs);
void remove (ArrayList xs);