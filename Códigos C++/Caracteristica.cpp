#ifndef CARACTERISTICAS
#define CARACTERISTICAS

#include <iostream>
#include <list>
#include <string>

using namespace std;

class Caracteristica{
private:
	string nome;
	string valor;
public:
	//construtor
	Caracteristica(){};
	
	//Detrutor
	~Caracteristica(){};
	
	void set_nome(string auxString)
	{
		nome=auxString;
	};
	
	void set_valor(string auxString)
	{
		valor=auxString;
	};
	
	string get_nome()
	{
		return nome;
	};
	
	bool verificar_val_car()
	{
		if (nome.empty() || valor.empty())
		{
			return false;
		}
		else
		{
			return true;
		}
	};
	
	string get_valor()
	{
		return valor;
	};
};

#endif
