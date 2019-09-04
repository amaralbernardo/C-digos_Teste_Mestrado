#include <iostream>
#include <list>
#include <string>
#include "Caracteristica.cpp"
#include "DTC_EB.hpp"
#include "Acentos.cpp"

using namespace std;

void DTC_EB::add_carac(string auxString)
{
	Caracteristica car_aux;
	car_aux.set_nome(auxString);
	list_carac.push_back(car_aux);
};

list<string> DTC_EB::get_carac()
{
	list<string> list_aux;
	list<Caracteristica>::iterator it_lis_car;
	for (it_lis_car=list_carac.begin(); it_lis_car!=list_carac.end(); it_lis_car++)
	{
		list_aux.push_back((*it_lis_car).get_nome());
	}
	return list_aux;
}

int DTC_EB::get_num_car()
{
	return list_carac.size();
};

void DTC_EB::add_valor(string auxString, int pos_val)
{
	list<Caracteristica>::iterator it_lis_car=list_carac.begin();
	advance(it_lis_car, pos_val);
	(*it_lis_car).set_valor(auxString);
};

bool DTC_EB::comparar_tamanhos()
{
	string val_str, car_str;
	list<Caracteristica>::iterator it_lis_car=list_carac.begin();
	bool certo=true;
	while (certo && it_lis_car!=list_carac.end())
	{
		certo=(*it_lis_car).verificar_val_car();
		it_lis_car++;
	}
	return certo;
}

bool DTC_EB::comparar_car(list<string> lista_comp)
{
	if (lista_comp.size()!=list_carac.size())
	{
		return false;
	}
	else
	{
		string car_str;
		list<Caracteristica>::iterator it_lis_car=list_carac.begin();
		list<string>::iterator it_str=lista_comp.begin();
		bool certo=true;
		while (certo && it_lis_car!=list_carac.end())
		{
			if ((*it_lis_car).get_nome()!=(*it_str))
			{
				certo=false;
			}
			else
			{
				it_lis_car++;
				it_str++;
			}
		}
		return certo;
	}
};

list<string> DTC_EB::get_valor()
{
	list<string> list_aux;
	list<Caracteristica>::iterator it_lis_car;
	for (it_lis_car=list_carac.begin(); it_lis_car!=list_carac.end(); it_lis_car++)
	{
		list_aux.push_back((*it_lis_car).get_valor());
	}
	return list_aux;
}

bool DTC_EB::comparar_val(list<string> lista_comp)
{
	string car_str;
	list<Caracteristica>::iterator it_lis_car=list_carac.begin();
	list<string>::iterator it_str=lista_comp.begin();
	bool certo=true;
	while (certo && it_lis_car!=list_carac.end())
	{
		if ((*it_lis_car).get_valor()!=(*it_str))
		{
			certo=false;
		}
		else
		{
			it_lis_car++;
			it_str++;
		}
	}
	return certo;
};

void DTC_EB::escrever()
{
	Acentos ace;
	//Características
	list<Caracteristica>::iterator it_lis_car;
	//Escrever "<Característica>: <Valor da Característica>"
	for (it_lis_car = list_carac.begin(); it_lis_car!=list_carac.end(); it_lis_car++)
	{
		cout << ace.texto((*it_lis_car).get_nome()) << ": " << ace.texto((*it_lis_car).get_valor()) << endl;
	}
	cout << endl;
};

void DTC_EB::apagar_dados()
{
	list_carac.clear();
};
