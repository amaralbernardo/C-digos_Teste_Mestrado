#include <iostream>
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <string>
#include <sstream>
#include "Acentos.cpp"
#include "Caracteristica.cpp"
#include "DTC.hpp"
#include "EB.hpp"
#include "DTC_EB.hpp"
#include "Combinacao.hpp"
#include "Combinacoes.hpp"
#include "Rede.hpp"
#include "OS.cpp"
#include "Estrutura.hpp"

using namespace std;


int main(int argc, char *argv[])
{	
	//Criar semente para gerar valores aleatórios
	srand(time(NULL));
	
	Acentos ace;
	bool comb_adi=false, dtc_adi=false, ebs_adi=false;
	list<string> dtc_car, eb_car, dtc_eb_car;
	string aux_string, opc_str, nome_fich="";
	Combinacoes comb;
	Estrutura sistema;
	int opc_esc, opc_OS;
	ifstream fich;
	char * pEnd = NULL;
	
	//Necessário introduzir um ficheiro de combinações, de DTCs e EBs para as leituras válidos
	while (!comb_adi || !dtc_adi || !ebs_adi)
	{
		//Caso se pretende adicionar um novo ficheiro de combinações
		if (!comb_adi)
		{
			dtc_adi=false;
			ebs_adi=false;
		}
		//Caso se pretende adicionar um novo ficheiro de DTCs
		else if (!dtc_adi)
		{
			ebs_adi=false;
		}
		//Adicionar ficheiro de combinações
		while (!comb_adi)
		{
			cout << "Introduza o nome do ficheiro das combina" << ace.texto("çõ") << "es a utilizar para as leituras: "; 
			getline(cin,aux_string);
			comb_adi=comb.abrir_ficheiro(aux_string, false, dtc_car, eb_car, dtc_eb_car);
			if (comb_adi)
			{
				dtc_car=comb.get_car_DTC();
				eb_car=comb.get_car_EB();
				dtc_eb_car=comb.get_car_DTC_EB();
			}
		}
		while (!dtc_adi)
		{
			cout << "Nome do ficheiro DTC a introduzir: ";
			getline(cin,aux_string);
			opc_esc=sistema.abrir_ficheiro_DTC(aux_string, dtc_car);
			if (opc_esc==2)
			{
				dtc_adi=true;
			}
			else if (opc_esc==4)
			{
				comb.apagar_dados();
				dtc_adi=true;
				ebs_adi=true;
				comb_adi=false;
			}
		}
		while (!ebs_adi)
		{
			cout << "Nome do ficheiro EB a introduzir: ";
			getline(cin,aux_string);
			opc_esc=sistema.abrir_ficheiro_EB(aux_string,eb_car,dtc_eb_car);
			if (opc_esc==2)
			{
				ebs_adi=true;
			}
			else if (opc_esc==4)
			{
				dtc_adi=false;
				ebs_adi=true;
			}
			else if (opc_esc==5)
			{
				comb.apagar_dados();
				ebs_adi=true;
				comb_adi=false;
			}
		}
	}
	sistema.add_prop(comb,true);
	comb.apagar_dados();
	while (opc_OS!=1 && opc_OS!=2)
	{
		cout << "Pretende adicionar OS " << ace.texto("à") << " estrutura introduzida?" << endl << "1. Sim" << endl << ace.texto("2. Não") << endl << endl;
		getline(cin,opc_str);
		opc_OS=strtol(opc_str.c_str(),&pEnd,10);
		if (*pEnd)
		{
			cout << ace.texto("Opção inválida!") << endl << endl;
			opc_OS=0;
		}
		else if (opc_OS!=1 && opc_OS!=2)
		{
			cout << ace.texto("Opção inválida!") << endl << endl;
		}
	}
	if (opc_OS==1)
	{
		comb_adi=false;
		while (!comb_adi)
		{
			cout << "Introduza o nome do ficheiro das combina" << ace.texto("çõ") << "es a utilizar para as OS: ";
			getline(cin,aux_string);
			comb_adi=comb.abrir_ficheiro(aux_string, true, dtc_car, eb_car, dtc_eb_car);
			if (!comb_adi)
			{
				opc_esc=0;
				while (opc_esc!=1 && opc_esc!=2)
				{
					cout << "Pretende inserir um novo ficheiro?" << endl << "1. Sim" << endl << ace.texto("2. Não") << endl << endl;
					getline(cin,opc_str);
					opc_esc=strtol(opc_str.c_str(),&pEnd,10);
					if (*pEnd)
					{
						cout << ace.texto("Opção inválida!") << endl << endl;
						opc_OS=0;
					}
					else if (opc_OS!=1 && opc_OS!=2)
					{
						cout << ace.texto("Opção inválida!") << endl << endl;
					}
				}
				if (opc_esc==2)
				{
					comb_adi=true;
					opc_OS=2;
				}
			}
		}
	}
	if (opc_OS==1)
	{
		opc_esc=3;
		while (opc_esc==3)
		{
			cout << "Introduza o nome do ficheiro das OS a introduzir no sistema: ";
			getline(cin,aux_string);
			opc_esc=sistema.abrir_ficheiro_OS(aux_string);
			if (opc_esc==2)
			{
				sistema.add_prop(comb, false);
			}
			else if (opc_esc==4)
			{
				opc_OS=2;
			}
		}
	}
	while (nome_fich.empty())
	{
		cout << "Introduza o nome com o qual pretende gravar a "<< ace.texto("simulação: "); 
		getline(cin,nome_fich);
		if (nome_fich=="")
		{
			cout << ace.texto("Não foi inserido nenhum nome!") << endl << endl;
		}
		else
		{
			fich.open(nome_fich+"_leitura.csv");
			if (fich.is_open())
			{
				fich.close();
				opc_esc=0;
				while (opc_esc!=1 && opc_esc!=2)
				{
					cout << "J" << ace.texto("á") << " existe um ficheiro com esse nome. Pretende substitu" << ace.texto("í") << "-lo?" << endl << "1. Sim" << endl << "2. Inserir um novo nome" << endl << endl;
					getline(cin,opc_str);
					opc_esc=strtol(opc_str.c_str(),&pEnd,10);
					if (*pEnd)
					{
						cout << ace.texto("Opção inválida!") << endl << endl;
						opc_esc=0;
					}
					else if (opc_OS!=1 && opc_OS!=2)
					{
						cout << ace.texto("Opção inválida!") << endl << endl;
					}
				}
				if (opc_esc==2)
				{
					nome_fich="";
				}
			}
		}
	}
	sistema.simular_leituras(nome_fich+"_leitura.csv");
	if (opc_OS==1)
	{
		sistema.simular_OS(nome_fich+"_OS.csv");
	}
	return 0;
};
