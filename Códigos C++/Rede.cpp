#include <iostream>
#include <string>
#include <list>
#include <algorithm>
#include "Caracteristica.cpp"
#include "DTC.hpp"
#include "EB.hpp"
#include "DTC_EB.hpp"
#include "Combinacao.hpp"
#include "Combinacoes.hpp"
#include "Rede.hpp"

using namespace std;

void Rede::add_car_DTC(string auxString)
{
	dtc_rede.add_carac(auxString);
};

bool Rede::comparar_car_DTC(list<string> lista_comp)
{
	return dtc_rede.comparar_car(lista_comp);
};

int Rede::get_num_car_DTC()
{
	return dtc_rede.get_num_car();
};

void Rede::add_val_DTC(string auxString, int pos_val)
{
	dtc_rede.add_valor(auxString, pos_val);
};

bool Rede::verificar_ID_EB(string auxString)
{
	list<string>::iterator it;
	it=find(id_eb.begin(),id_eb.end(),auxString);
	if (it==id_eb.end())
	{
		return true;
	}
	return false;
};

void Rede::add_lista_eb(EB auxEB)
{
	lista_eb.push_back(auxEB);
};

void Rede::add_lista_dtc_eb(DTC_EB auxDTC_EB)
{
	lista_dtc_eb.push_back(auxDTC_EB);
};

void Rede::add_id_EB(string auxID)
{
	id_eb.push_back(auxID);
};

bool Rede::verificar_rede()
{
	if (!lista_eb.empty())
	{
		return true;
	}
	return false;
};

void Rede::apagar_EBs()
{
	lista_eb.clear();
};

void Rede::apagar_DTC_EBs()
{
	lista_dtc_eb.clear();
};

void Rede::apagar_ID_EBs()
{
	id_eb.clear();
};

void Rede::add_prop(Combinacoes comb)
{
	list<EB>::iterator it_eb;
	list<DTC_EB>::iterator it_dtc_eb;
	int num_EB=lista_eb.size(), i; //num_EB -> Número de ebs associadas ao DTC
	list<double> prop_aux;
	//Para cada combinação DTC + EB, calcular a sua proporção
	for (i=0; i<num_EB; i++)
	{
		it_eb=lista_eb.begin();
		it_dtc_eb=lista_dtc_eb.begin();
		advance(it_eb,i);
		advance(it_dtc_eb,i);
		
		//Calcular a proporção para todos os dias da semana
		prop_aux=comb.get_prop_leituras(dtc_rede, *it_eb, *it_dtc_eb, num_EB);
		
		//Adicionar proporções 
		lista_prop.push_back(prop_aux);
	}
};

DTC Rede::get_dtc_rede()
{
	return dtc_rede;
};

list<string> Rede::get_id_eb()
{
	return id_eb;
};

EB Rede::get_EB(int pos)
{
	list<EB>::iterator it_EB=lista_eb.begin();
	advance(it_EB, pos);
	return *it_EB;
};

DTC_EB Rede::get_DTC_EB(int pos)
{
	list<DTC_EB>::iterator it_DTC_EB=lista_dtc_eb.begin();
	advance(it_DTC_EB, pos);
	return *it_DTC_EB;
};

int Rede::get_num_EBs()
{
	return lista_eb.size();
};

list<EB> Rede::get_lista_eb()
{
	return lista_eb;
}

list<DTC_EB> Rede::get_lista_dtc_eb()
{
	return lista_dtc_eb;
}

list<list<double>> Rede::get_lista_prop()
{
	return lista_prop;
};

list<string> Rede::get_car_DTC()
{
	return dtc_rede.get_carac();
};

list<string> Rede::get_car_EB()
{
	return lista_eb.front().get_carac();
};

list<string> Rede::get_car_DTC_EB()
{
	return lista_dtc_eb.front().get_carac();
};
