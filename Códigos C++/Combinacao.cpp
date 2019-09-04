#include <iostream>
#include <string>
#include "Caracteristica.cpp"
#include "DTC.hpp"
#include "EB.hpp"
#include "DTC_EB.hpp"
#include "Combinacao.hpp"

using namespace std;

void Combinacao::add_car_DTC(string auxString)
{
	dtc_comb.add_carac(auxString);
};

void Combinacao::add_car_EB(string auxString)
{
	eb_comb.add_carac(auxString);
};

void Combinacao::add_car_DTC_EB(string auxString)
{
	dtc_eb_comb.add_carac(auxString);
};

list<string> Combinacao::get_car_DTC()
{
	return dtc_comb.get_carac();
}

list<string> Combinacao::get_car_EB()
{
	return eb_comb.get_carac();
}

list<string> Combinacao::get_car_DTC_EB()
{
	return dtc_eb_comb.get_carac();
}

int Combinacao::get_num_car_DTC()
{
	return dtc_comb.get_num_car();
}

int Combinacao::get_num_car_EB()
{
	return eb_comb.get_num_car();
}

int Combinacao::get_num_car_DTC_EB()
{
	return dtc_eb_comb.get_num_car();
}

void Combinacao::add_val_DTC(string auxString, int pos_val)
{
	dtc_comb.add_valor(auxString, pos_val);
}

void Combinacao::add_val_EB(string auxString, int pos_val)
{
	eb_comb.add_valor(auxString, pos_val);
}

void Combinacao::add_val_DTC_EB(string auxString, int pos_val)
{
	dtc_eb_comb.add_valor(auxString, pos_val);
}

void Combinacao::set_dia_semana(string auxString)
{
	dia_semana=auxString;
}

void Combinacao::set_nome_OS(string auxString)
{
	nome_OS=auxString;
}

void Combinacao::set_qtd_EBs(int num_min, int num_max)
{
	num_min_EBs=num_min;
	num_max_EBs=num_max;
}

void Combinacao::set_prop(double prop_aux)
{
	prop=prop_aux;
}

bool Combinacao::comparar_tamanhos_DTC()
{
	return dtc_comb.comparar_tamanhos();
};

bool Combinacao::comparar_tamanhos_EB()
{
	return eb_comb.comparar_tamanhos();
};

bool Combinacao::comparar_tamanhos_DTC_EB()
{
	return dtc_eb_comb.comparar_tamanhos();
};

list<string> Combinacao::get_val_DTC()
{
	return dtc_comb.get_valor();
}

list<string> Combinacao::get_val_EB()
{
	return eb_comb.get_valor();
}

list<string> Combinacao::get_val_DTC_EB()
{
	return dtc_eb_comb.get_valor();
}

string Combinacao::get_dia_semana()
{
	return dia_semana;
};

int Combinacao::get_num_min_EBs()
{
	return num_min_EBs;
};

int Combinacao::get_num_max_EBs()
{
	return num_max_EBs;
};

double Combinacao::get_prop()
{
	return prop;
};

void Combinacao::criar_combinacao(DTC dtc_aux, EB eb_aux , DTC_EB dtc_eb_aux, string ds_aux, string n_os_aux, int num_min_eb_aux, int num_max_eb_aux, double prop_aux)
{
	list<string> str_lis;
	list<string>::iterator it_str;
	int pos_val=0;
	
	//Dia da semana
	dia_semana=ds_aux;
	
	//Nome da OS
	nome_OS=n_os_aux;
	
	//Número mínimo de EBs
	num_min_EBs=num_min_eb_aux;
	
	//Número máximo de EBs
	num_max_EBs=num_max_eb_aux;
	
	//Proporção
	prop=prop_aux;
	
	//Características do DTC
	str_lis=dtc_aux.get_carac();
	for (it_str=str_lis.begin(); it_str!=str_lis.end(); it_str++)
	{
		dtc_comb.add_carac(*it_str);
	}
	
	//Valor das características do DTC
	str_lis=dtc_aux.get_valor();
	for (it_str=str_lis.begin(); it_str!=str_lis.end(); it_str++)
	{
		dtc_comb.add_valor(*it_str, pos_val);
		pos_val++;
	}
	pos_val=0;
	
	//Características do EB
	str_lis=eb_aux.get_carac();
	for (it_str=str_lis.begin(); it_str!=str_lis.end(); it_str++)
	{
		eb_comb.add_carac(*it_str);
	}
	
	//Valor das características do EB
	str_lis=eb_aux.get_valor();
	for (it_str=str_lis.begin(); it_str!=str_lis.end(); it_str++)
	{
		eb_comb.add_valor(*it_str, pos_val);
		pos_val++;
	}
	pos_val=0;
	
	//Características do DTC + EB
	str_lis=dtc_eb_aux.get_carac();
	for (it_str=str_lis.begin(); it_str!=str_lis.end(); it_str++)
	{
		dtc_eb_comb.add_carac(*it_str);
	}
	
	//Valor das características do DTC + EB
	str_lis=dtc_eb_aux.get_valor();
	for (it_str=str_lis.begin(); it_str!=str_lis.end(); it_str++)
	{
		dtc_eb_comb.add_valor(*it_str, pos_val);
		pos_val++;
	}
};

void Combinacao::apagar_dados()
{
	dtc_comb.apagar_dados();
	eb_comb.apagar_dados();
	dtc_eb_comb.apagar_dados();
};

string Combinacao::get_nome_OS()
{
	return nome_OS;
};
