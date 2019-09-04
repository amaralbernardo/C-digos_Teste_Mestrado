#include <iostream>
#include <cstdlib>
#include <string>
#include <list>
#include <sstream>
#include <fstream>
#include <algorithm>
#include <utility>
#include <chrono>
#include "Caracteristica.cpp"
#include "DTC_EB.hpp"
#include "EB.hpp"
#include "DTC.hpp"
#include "Combinacao.hpp"
#include "Combinacoes.hpp"
#include "Rede.hpp"
#include "OS.cpp"
#include "Estrutura.hpp"
#include "Acentos.cpp"

using namespace std;

Rede Estrutura::verificar_car_DTC(string linha, list<string> combi_DTC)
{
	Acentos ace;
	string auxString, esp_brancos;
	stringstream lineStream;
	Rede rede;
	bool existe_ID=false;
	
	//Transforma a string numa stream para obter as informações da linha mais facilmente
	lineStream.str(linha);
	
	//Identifica quais as caracteristicas do DTC existentes
	while (getline(lineStream, auxString, ';'))
	{
		//Apaga os espaços em branco para verificar se a string obtida é ""
		esp_brancos=ace.espacos_branco(auxString);
		
		//O ID que identifica o DTC deve ser a última caracteristica a ser introduzida. Se não for a última, o ficheiro não é válido
		if (existe_ID)
		{
			throw fich_vali_ID_meio();
		}
		//Se esp_brancos="", então o ficheiro não é valido
		if (esp_brancos=="")
		{
			throw fich_vali_car_vazio();
		}
		//Se a string não for "ID", então é uma caracteristica do DTC
		if (auxString!="ID")
		{
			rede.add_car_DTC(auxString);
		}
		//A variável existe_ID passa a true no momento em que é encontrado a característica "ID"
		else
		{
			existe_ID=true;
		}
	}
	//Se o ficheiro não tiver a característica ID, o ficheiro não é válido
	if (!existe_ID)
	{
		throw fich_vali_falta_ID();
	}
	//As características do DTC a testar têm de ser as mesmas das características do ficheiro das combinações introduzido anteriormente. Se não forem iguais, o ficheiro não é válido
	if (!rede.comparar_car_DTC(combi_DTC))
	{
		throw fich_vali_car_dif();
	}
	return rede;
};

bool Estrutura::verificar_ID_DTC(string auxString)
{
	list<string>::iterator it;
	it=find(id_rede.begin(),id_rede.end(),auxString);
	if (it==id_rede.end())
	{
		return true;
	}
	return false;
}

string Estrutura::verificar_val_DTC(string linha, Rede rede, int linha_pos)
{
	Acentos ace;
	string auxString, str_error, id_aux, esp_brancos;
	stringstream lineStream;
	bool valido=false;
	int pos=0, num_carac=rede.get_num_car_DTC(); //num_carac -> Número de caracteristicas no DTC
	
	//Transforma a string numa stream para obter as informações da linha mais facilmente
	lineStream.str(linha);
	
	//Identifica os valores das caracteristicas existentes
	while (getline(lineStream, auxString,';'))
	{
		//Apaga os espaços em branco para verificar se a string obtida é ""
		esp_brancos=ace.espacos_branco(auxString);
		
		//Se houver mais características na linha do que aquelas definidas, a linha não é válida
		if (valido)
		{
			str_error = "A estrutura do ficheiro para a leitura dos DTCs não está bem definida na linha " + to_string(linha_pos);
			return str_error;
		}
		//Se auxString="", então a linha não é valida
		if (esp_brancos=="")
		{
			str_error = "Na linha "+to_string(linha_pos)+" existe uma característica sem valor!";
			return str_error;
		}
		//Enquanto for característica adicionar os valores ao DTC
		if (pos<num_carac)
		{
			rede.add_val_DTC(auxString, pos);
			pos++;
		}
		//O último valor é o ID
		else
		{
			//Se o ID já estiver associado a outro DTC, a linha não é válida
			if (!verificar_ID_DTC(auxString))
			{
				str_error = "O ID da linha "+to_string(linha_pos)+" já é utilizado por outro DTC definido no ficheiro numa das linhas anteriores!";
				return str_error;
			}
			id_aux=auxString;
			valido=true;
		}
	}
	//O programa verifica se a linha introduzida é válida
	if (!valido)
	{
		str_error = "A estrutura do ficheiro para a leitura dos DTCs não está bem definida na linha " + to_string(linha_pos);
		return str_error;
	}
	id_rede.push_back(id_aux);
	lis_rede.push_back(rede);
	return "";
};

int Estrutura::abrir_ficheiro_DTC(string nome_ficheiro, list<string> combi_DTC)
{
	Acentos ace;
	ifstream f_ent(nome_ficheiro);
	ofstream f_out;
	string aux_comb, erros_txt="", opc_str;
	Rede rede;
	bool carac=true, erros=false, erros_carac=false, erros_rede;
	int linha_pos=2, opc;
	char * pEnd=NULL;
	
	try
	{
		//Verifica se ficheiro existe
		if (!f_ent.is_open())
		{
			throw fich_exis();
		}
		//Cria o ficheiro 'log_erros_DTC'
		f_out.open("log_erros_DTC.txt");
		
		//Enquanto houver linhas para ler no ficheiro
		while (getline(f_ent, aux_comb, '\n'))
		{
			//Verificar se tem caracteres ilegais
			aux_comb=ace.car_esp(aux_comb);
			//Se carac=true, verifica as caracteristicas
			if (carac)
			{
				rede=verificar_car_DTC(aux_comb, combi_DTC);
				carac=false;
			}
			//Se carac=false, verifica os valores de cada DTC introduzido pelo utilizador
			else
			{
				erros_txt=verificar_val_DTC(aux_comb, rede, linha_pos);
				if (!erros_txt.empty())
				{
					erros=true;
					f_out << erros_txt << endl << endl;
				}
				linha_pos++;
			}
		}
		f_ent.close();
		f_out.close();
	}
	catch (fich_exis)
	{
		cout << "O ficheiro para a leitura dos DTCs " << ace.texto("não existe!") << endl;
		return 3;
	}
	catch (fich_vali_ID_meio)
	{
		f_ent.close();
		f_out.close();
		cout << "A vari" << ace.texto("á") << "vel 'ID' deve ser colocada na "<< ace.texto("última") << "coluna!" << endl;
		erros_carac=true;
	}
	catch (fich_vali_car_vazio)
	{
		f_ent.close();
		f_out.close();
		cout << "Existe um campo vazio entre as caracter" << ace.texto("í") << "sticas. O ficheiro n" << ace.texto("ã") << "o " << ace.texto("é") << " v" << ace.texto("á") << "lido" << endl;
		erros_carac=true;
	}
	catch (fich_vali_falta_ID)
	{
		f_ent.close();
		f_out.close();
		cout << "O ficheiro para a leitura dos DTCs " << ace.texto("não contém a variável ") << "'ID'!" << endl;
		erros_carac=true;
	}
	catch (fich_vali_car_dif)
	{
		f_ent.close();
		f_out.close();
		cout << "As caracter" << ace.texto("í") << "sticas dos DTCs " << ace.texto("não") << " coincidem com as das " << ace.texto("combincações!") << endl;
		erros_carac=true;
	}
	//Se houver erros no ficheiro em relação às características, o utilizador tem as seguintes opções:
	//opc=1 -> Introduzir um novo ficheiro de DTCs;
	//opc=2 -> Introduzir um novo ficheiro de combinações de leituras;
	if (erros_carac)
	{
		opc=0;
		while (opc<1 || opc>2)
		{
			cout << endl << "Existe erros nas " << ace.texto("características") << " no ficheiro introduzido. Pretende:" << endl << "1.Introduzir um novo ficheiro dos DTCs;" << endl << "2.Introduzir um novo ficheiro das combina" << ace.texto("ções;") << endl << endl;
			getline(cin,opc_str);
			opc=strtol(opc_str.c_str(),&pEnd,10);
			if (*pEnd)
			{
				cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
				opc=0;
			}
			else if (opc<1 || opc>2)
			{
				cout << endl << ace.texto("Opção inválida!") << endl << endl;
			}
		}
		opc+=2;
	}
	else if (lis_rede.empty())
	{
		if (erros)
		{
			cout << endl << ace.texto("Existem erros no ficheiro introduzido! Devido a esses erros não ");
		}
		else
		{
			cout << endl << ace.texto("Não ");
		}
		cout << ace.texto("foram inseridos nenhum DTC à estrutura! Por favor verifique o ficheiro introduzido!") << endl << endl;
		opc=1;
	}
	//Se houver erros no ficheiro em relação aos DTCs introduzidos, o utilizador tem as seguintes opções:
	//opc=1 -> Verificar o ficheiro dos erros
	//opc=2 -> Continuar com o programa
	//opc=3 -> Introduzir um novo ficheiro de DTCs;
	//opc=4 -> Introduzir um novo ficheiro de combinações de leituras;
	else if (erros)
	{
		opc=0;
		while (opc<1 || opc>4)
		{
			cout << endl << "Existe erros no ficheiro introduzido. Pretende:" << endl << "1.Verificar o ficheiro dos erros;" << endl << "2.Continuar com o programa;" << endl << "3.Introduzir um novo ficheiro dos DTCs;" << endl << "4.Introduzir um novo ficheiro das combina" << ace.texto("ções;") << endl << endl;
			getline(cin,opc_str);
			opc=strtol(opc_str.c_str(),&pEnd,10);
			if (*pEnd)
			{
				cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
				opc=0;
			}
			else if (opc<1 || opc>4)
			{
				cout << endl << ace.texto("Opção inválida!") << endl << endl;
			}
		}
		
		//Abrir o ficheiro dos erros criado durante a leitura do ficheiro
		if (opc==1)
		{
			f_ent.open("log_erros_DTC.txt");
			while (getline(f_ent, aux_comb, '\n'))
			{
				cout << ace.texto(aux_comb) << endl;
			}
			f_ent.close();
			opc=0;
			
			//Depois de abrir o ficheiro, o utilizador tem de escolher se pretende continuar com o programa, inserir um novo ficheiro de DTCs ou inserir um novo ficheiro de combinações
			while (opc<1 || opc>3)
			{
				cout << "Pretende:" << endl << "1.Continuar com o programa;" << endl << "2.Introduzir um novo ficheiro dos DTCs;" << endl << "3.Introduzir um novo ficheiro das combina" << ace.texto("ções") << endl << endl;
				getline(cin,opc_str);
				opc=strtol(opc_str.c_str(),&pEnd,10);
				if (*pEnd)
				{
					cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
					opc=0;
				}
				else if (opc<1 || opc>3)
				{
					cout << endl << ace.texto("Opção inválida!") << endl << endl;
				}
			}
			opc++;
		}
		
		//Antes de introduzir um novo ficheiro, é necessário apagar todas os valores introduzidos com o ficheiro atual
		if (opc==3 || opc==4)
		{
			id_rede.clear();
			lis_rede.clear();
		}
	}
	//Se não houver erros, continua-se com o programa
	else
	{
		opc=2;
	}
	return opc;
};

int Estrutura::verificar_associar_ID_DTC(string auxString)
{
	list<string>::iterator it;
	it=find(id_rede.begin(),id_rede.end(),auxString);
	if (it==id_rede.end())
	{
		return -1;
	}
	return distance(id_rede.begin(), it);
}

bool Estrutura::verificar_ID_EB(string auxString, int pos_ID)
{
	list<Rede>::iterator it_sub;
	it_sub=lis_rede.begin();
	advance(it_sub, pos_ID);
	return (*it_sub).verificar_ID_EB(auxString);
}

string Estrutura::verificar_val_EB(string linha, EB auxEB, DTC_EB auxDTC_EB, int num_car_EB, int num_car_DTC_EB, int linha_pos)
{
	Acentos ace;
	string auxString, esp_branco, id_aux, str_error="";
	stringstream lineStream;
	bool valido=false;
	int pos_ID, col=0;
	list<Rede>::iterator it_sub;
	
	//Transforma a string numa stream para obter as informações da linha mais facilmente
	lineStream.str(linha);
	
	//Identifica os valores das caracteristicas existentes
	while (getline(lineStream, auxString, ';'))
	{
		//Apaga os espaços em branco para verificar se a string obtida é ""
		esp_branco=ace.espacos_branco(auxString);
		
		//Se houver mais características na linha do que aquelas definidas, a linha não é válida
		if (valido)
		{
			str_error="A estrutura do ficheiro para a leitura das EBs não está bem definida na linha " + to_string(linha_pos);
			return str_error;
		}
		//Se esp_branco="", então a linha não é valida
		if (esp_branco=="")
		{
			str_error="Na linha " + to_string(linha_pos) + " existe uma característica sem valor!";
			return str_error;
		}
		//Caracteristica EB
		if (col<num_car_EB)
		{
			auxEB.add_valor(auxString, col);
		}
		//Caracteristica DTC+EB
		else if (col<num_car_EB+num_car_DTC_EB)
		{
			auxDTC_EB.add_valor(auxString, col-num_car_EB);
		}
		//O último valor é o ID
		else
		{
			//Se o ID não estiver associado a nenhum DTC, a linha não é válida (pos=-1)
			pos_ID=verificar_associar_ID_DTC(auxString);
			if (pos_ID==-1)
			{
				str_error="O elemento da linha "+to_string(linha_pos)+" não tem nenhum DTC associado!"; 
				return str_error;
			}
			getline(lineStream, auxString, ';');
			//Se o ID já estiver associado a outra EB, a linha não é válida
			if (!verificar_ID_EB(auxString,pos_ID))
			{
				str_error = "O ID da linha "+to_string(linha_pos)+" já é utilizado por outra EB dentro daquele DTC definido no ficheiro numa das linhas anteriores!";
				return str_error;
			}
			id_aux=auxString;
			valido=true;
		}
		col++;
	}
	if (!valido)
	{
		str_error="A estrutura do ficheiro para a leitura dos DTCs não está bem definida na linha " + to_string(linha_pos);
		return str_error;
	}
	it_sub=lis_rede.begin();
	advance(it_sub,pos_ID);
	(*it_sub).add_lista_eb(auxEB);
	(*it_sub).add_lista_dtc_eb(auxDTC_EB);
	(*it_sub).add_id_EB(id_aux);
	return "";
};

bool Estrutura::verificar_Estrutura()
{
	list<Rede>::iterator it_sub;
	list<string>::iterator it_str;
	bool valido, erros=false;
	ofstream f_out;
	int pos;
	f_out.open("log_erros_Estrutura.txt");
	
	it_sub=lis_rede.begin();
	while(it_sub!=lis_rede.end())
	{
		valido=(*it_sub).verificar_rede();
		if (!valido)
		{
			pos=distance(lis_rede.begin(), it_sub);
			it_str=id_rede.begin();
			advance(it_str,pos);
			f_out << "O DTC '" << *it_str << "' foi desconsiderado pois não tinha nenhum EB associado!" << endl << endl;
			it_sub=lis_rede.erase(it_sub);
			it_str=id_rede.erase(it_str);
			erros=true;
		}
		else
		{
			it_sub++;
		}
	}
	f_out.close();
	return erros;
}

int Estrutura::abrir_ficheiro_EB(string nome_ficheiro, list<string> combi_EB, list<string> combi_DTC_EB)
{
	Acentos ace;
	ifstream f_ent(nome_ficheiro);
	ofstream f_out;
	stringstream lineStream;
	string aux_comb, esp_branco, auxString, erros_txt="", opc_str;
	bool carac=true, existe_ID=false, erros=false, erros_carac=false, erros_estrutura=false;
	EB auxEB;
	DTC_EB auxDTC_EB;
	list<Rede>::iterator it_sub;
	//num_car_EB -> Número de características EB
	//num_car_DTC_EB -> Número de características DTC+EB
	int col=1, num_car_EB=combi_EB.size(), num_car_DTC_EB=combi_DTC_EB.size(), linha_pos=2, opc;
	char * pEnd=NULL;
	
	try
	{
		if (!f_ent.is_open())
		{
			throw fich_exis();
		}
		f_out.open("log_erros_EB.txt");
		while (getline(f_ent, aux_comb, '\n'))
		{
			//Verificar se tem caracteres ilegais
			aux_comb=ace.car_esp(aux_comb);
			if (carac)
			{
				//Transforma a string numa stream para obter as informações da linha mais facilmente
				lineStream.str(aux_comb);
				//Identifica quais as caracteristicas das EBs e DTCs+EBs existentes
				while (getline(lineStream, auxString, ';'))
				{
					//Apaga os espaços em branco para verificar se a string obtida é ""
					esp_branco=ace.espacos_branco(auxString);
					
					//O ID que identifica a qual DTC a EB pertence deve ser a última caracteristica a ser introduzida. Se não for a última, o ficheiro não é válido
					if (existe_ID)
					{
						throw fich_vali_ID_meio();
					}
					//Se esp_branco="", então o ficheiro não é valido
					if (esp_branco=="")
					{
						throw fich_vali_car_vazio();
					}
					//A variável existe_ID passa a true no momento em que é encontrado a característica "ID"
					if (auxString=="ID")
					{
						getline(lineStream, auxString, '\n');
						if (auxString=="ID_EB")
						{
							existe_ID=true;
						}
						else
						{
							throw fich_vali_falta_ID_EB();
						}
					}
					//Caracteristica da EB
					else if (col<=num_car_EB)
					{
						auxEB.add_carac(auxString);
					}
					//Caracteristica da EB+DTC
					else if (col<=num_car_EB+num_car_DTC_EB)
					{
						auxDTC_EB.add_carac(auxString);
					}
					//Se ultrapassou o número de características, então o ficheiro não é válido
					else
					{
						throw fich_vali_car_mais();
					}
					col++;
				}
				//Se o ficheiro não tiver a característica ID, o ficheiro não é válido
				if (!existe_ID)
				{
					throw fich_vali_falta_ID();
				}
				//As características da EB a testar têm de ser as mesmas das características do ficheiro das combinações introduzido anteriormente. Se não forem iguais, o ficheiro não é válido
				if (!auxEB.comparar_car(combi_EB) || !auxDTC_EB.comparar_car(combi_DTC_EB))
				{
					throw fich_vali_car_dif();
				}
				carac=false;
				
				//limpa a stream
				lineStream.clear();
			}
			else
			{
				erros_txt=verificar_val_EB(aux_comb, auxEB, auxDTC_EB, num_car_EB, num_car_DTC_EB, linha_pos);
				if (!erros_txt.empty())
				{
					erros=true;
					f_out << erros_txt << endl << endl;
				}
				linha_pos++;
			}
		}
		f_ent.close();
		f_out.close();
		erros_estrutura=verificar_Estrutura();
	}
	catch (fich_exis)
	{
		cout << "O ficheiro para a leitura dos EBs " << ace.texto("não existe!") << endl;
		return 3;
	}
	catch (fich_vali_ID_meio)
	{
		f_ent.close();
		f_out.close();
		cout << "A vari" << ace.texto("á") << "vel 'ID' deve ser colocada na "<< ace.texto("última") << "coluna!" << endl;
		erros_carac=true;
	}
	catch (fich_vali_car_vazio)
	{
		f_ent.close();
		f_out.close();
		cout << "Existe um campo vazio entre as caracter" << ace.texto("í") << "sticas. O ficheiro n" << ace.texto("ã") << "o " << ace.texto("é") << " v" << ace.texto("á") << "lido" << endl;
		erros_carac=true;
	}
	catch (fich_vali_falta_ID_EB)
	{
		f_ent.close();
		f_out.close();
		cout << "O ficheiro para a leitura das EBs " << ace.texto("não contém a variável ") << "'ID_EB'!" << endl;
		erros_carac=true;
	}
	catch (fich_vali_car_dif)
	{
		f_ent.close();
		f_out.close();
		cout << "As caracter" << ace.texto("í") << "sticas das EBs " << ace.texto("não") << " coincidem com as das " << ace.texto("combincações!") << endl;
		erros_carac=true;
	}
	catch (fich_vali_falta_ID)
	{
		f_ent.close();
		f_out.close();
		cout << "O ficheiro para a leitura das EBs " << ace.texto("não contém a variável ") << "'ID'!" << endl;
		erros_carac=true;
	}
	//Se algum DTC for apagado por não ter nenhuma EB associada, o programa irá verificar se existe alguma na estrutura. Se não existir, o programa não continuará
	if (erros_estrutura)
	{
		erros=false;
		if (lis_rede.empty())
		{
			cout << endl << ace.texto("Todos os DTCs inseridos não possuiam EBs e portanto foram removidos! A estrutura não é válida! É necessário introduzir novos ficheiros para as EBs e DTCs!") << endl << endl;
			opc=4;
		}
		//Se os DTCs não forem todos apagados, o utilizador tem as seguintes opções:
		//opc=1 -> Verificar DTCs apagados e erros no ficheiro;
		//opc=2 -> Continuar com o programa;
		//opc=3 -> Introduzir um novo ficheiro de DTCs;
		//opc=4 -> Introduzir um novo ficheiro de combinações de leituras;
		else
		{
			opc=0;
			while (opc<1 || opc>4) 
			{
				cout << endl << ace.texto("Houve DTCs apagados devido à falta de associação de EBs. Pretende:") << endl << "1.Verificar DTCs apagados e ficheiro dos erros;" << endl << "2.Continuar com o programa;" << endl << "3.Introduzir um novo ficheiro dos DTCs;" << endl << "4.Introduzir um novo ficheiro das combina" << ace.texto("ções;") << endl << endl;
				getline(cin,opc_str);
				opc=strtol(opc_str.c_str(),&pEnd,10);
				if (*pEnd)
				{
					cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
					opc=0;
				}
				else if (opc<1 || opc>4)
				{
					cout << endl << ace.texto("Opção inválida!") << endl << endl;
				}
				if (opc==3 || opc==4)
				{
					opc++;
				}
			}
			//Abrir o ficheiro com os DTCs apagados e erros encontrados no ficheiro inserido
			if (opc==1)
			{
				f_ent.open("log_erros_Estrutura.txt");
				while (getline(f_ent, aux_comb, '\n'))
				{
					cout << ace.texto(aux_comb) << endl;
				}
				f_ent.close();
				f_ent.open("log_erros_EB.txt");
				while (getline(f_ent, aux_comb, '\n'))
				{
					cout << ace.texto(aux_comb) << endl;
				}
				opc=0;
			
				//Depois de abrir o ficheiro, o utilizador tem de escolher se pretende continuar com o programa, inserir um novo ficheiro de DTCs ou inserir um novo ficheiro de combinações
				while (opc<1 || opc>3)
				{
					cout << "Pretende:" << endl << "1.Continuar com o programa;" << endl << "2.Introduzir um novo ficheiro dos DTCs;" << endl << "3.Introduzir um novo ficheiro das combina" << ace.texto("ções;") << endl << endl;
					getline(cin,opc_str);
					opc=strtol(opc_str.c_str(),&pEnd,10);
					if (*pEnd)
					{
						cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
						opc=0;
					}
					if (opc<1 || opc>3)
					{
						cout << endl << ace.texto("Opção inválida!") << endl << endl;
					}
				}
				if (opc!=1)
				{
					opc+=2;
				}
				else
				{
					opc++;
				}
			}
			//Antes de introduzir um novo ficheiro, é necessário apagar todas os valores introduzidos com o ficheiro atual
			if (opc!=2)
			{
				for (it_sub=lis_rede.begin(); it_sub!=lis_rede.end(); it_sub++)
				{
					(*it_sub).apagar_EBs();
					(*it_sub).apagar_DTC_EBs();
					(*it_sub).apagar_ID_EBs();
				}
				id_rede.clear();
				lis_rede.clear();
			}
		}
	}
	
	//Se houver erros no ficheiro em relação às características, o utilizador tem as seguintes opções:
	//opc=1 -> Introduzir um novo ficheiro de EBs;
	//opc=2 -> Introduzir um novo ficheiro de DTCs;
	//opc=3 -> Introduzir um novo ficheiro de combinações de leituras;
	else if (erros_carac)
	{
		opc=0;
		while (opc<1 || opc>3)
		{
			cout << endl << "Existe erros nas " << ace.texto("características") << " no ficheiro introduzido. Pretende:" << endl << "1.Introduzir um novo ficheiro dos EBs;" << endl << "2.Introduzir um novo ficheiro dos DTCs;" << endl << "3.Introduzir um novo ficheiro das combina" << ace.texto("ções;") << endl << endl;
			getline(cin,opc_str);
			opc=strtol(opc_str.c_str(),&pEnd,10);
			if (*pEnd)
			{
				cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
				opc=0;
			}
			else if (opc<1 || opc>3)
			{
				cout << endl << ace.texto("Opção inválida!") << endl << endl;
			}
		}
		opc+=2;
	}
	//Se houver erros no ficheiro em relação aos EBs introduzidos, o utilizador tem as seguintes opções:
	//opc=1 -> Verificar o ficheiro dos erros
	//opc=2 -> Continuar com o programa
	//opc=3 -> Introduzir um novo ficheiro de EBs;
	//opc=4 -> Introduzir um novo ficheiro de DTCs;
	//opc=5 -> Introduzir um novo ficheiro de combinações de leituras;
	else if (erros)
	{
		opc=0;
		while (opc<1 || opc>5)
		{
			cout << endl << "Existe erros no ficheiro introduzido. Pretende:" << endl << "1.Verificar o ficheiro dos erros;" << endl << "2.Continuar com o programa;" << endl << "3.Introduzir um novo ficheiro dos EBs;" << endl << "4.Introduzir um novo ficheiro dos DTCs;" << endl << "5.Introduzir um novo ficheiro das combina" << ace.texto("ções;") << endl << endl;
			getline(cin,opc_str);
			opc=strtol(opc_str.c_str(),&pEnd,10);
			if (*pEnd)
			{
				cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
				opc=0;
			}
			if (opc<1 || opc>5)
			{
				cout << endl << ace.texto("Opção inválida!") << endl << endl;
			}
		}
		//Abrir o ficheiro dos erros criado durante a leitura do ficheiro
		if (opc==1)
		{
			f_ent.open("log_erros_EB.txt");
			while (getline(f_ent, aux_comb, '\n'))
			{
				cout << ace.texto(aux_comb) << endl;
			}
			f_ent.close();
			opc=0;
			
			//Depois de abrir o ficheiro, o utilizador tem de escolher se pretende continuar com o programa, inserir um novo ficheiro de EBs, inserir um novo ficheiro de DTCs ou inserir um novo ficheiro de combinações
			while (opc<1 || opc>4)
			{
				cout << "Pretende:" << endl << "1.Continuar com o programa;" << endl << "2.Introduzir um novo ficheiro dos EBs;" << endl << "3.Introduzir um novo ficheiro dos DTCs;" << endl << "4.Introduzir um novo ficheiro das combina" << ace.texto("ções;") << endl << endl;
				getline(cin,opc_str);
				opc=strtol(opc_str.c_str(),&pEnd,10);
				if (*pEnd)
				{
					cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
					opc=0;
				}
				if (opc<1 || opc>4)
				{
					cout << endl << ace.texto("Opção inválida!") << endl << endl;
				}
			}
			opc++;
		}
		//Antes de introduzir um novo ficheiro, é necessário apagar todas os valores introduzidos com o ficheiro atual
		if (opc!=2)
		{
			for (it_sub=lis_rede.begin(); it_sub!=lis_rede.end(); it_sub++)
			{
				(*it_sub).apagar_EBs();
				(*it_sub).apagar_DTC_EBs();
				(*it_sub).apagar_ID_EBs();
			}
			if (opc==4 || opc==5)
			{
				id_rede.clear();
				lis_rede.clear();
			}
		}
	}
	else
	{
		opc=2;
	}
	return opc;
};

void Estrutura::add_prop(Combinacoes comb, bool leituras)
{
	if (leituras)
	{
		list<Rede>::iterator it_sub;
		//Para cada rede (conjunto de DTC mais as EBs associadas a esse DTC), dar as proporções para cada combinação DTC + EB
		for (it_sub=lis_rede.begin(); it_sub!=lis_rede.end(); it_sub++)
		{
			(*it_sub).add_prop(comb);
		}
	}
	else
	{
		list<OS>::iterator it_os;
		list<string>::iterator it_str;
		list<Rede>::iterator it_sub;
		double prop_aux;
		int pos, num_EBs;
		list<string> ids_ebs;
		DTC dtc_aux;
		EB eb_aux;
		DTC_EB dtc_eb_aux;
		string id_DTC_aux, id_EB_aux, ds, n_os;
		//Para cada OS, dar as proporções
		for (it_os=lis_os.begin(); it_os!=lis_os.end(); it_os++)
		{
			n_os=(*it_os).get_nome_OS();
			id_DTC_aux=(*it_os).get_id_DTC();
			id_EB_aux=(*it_os).get_id_EB();
			ds=(*it_os).get_dia_semana();
			
			it_str=find(id_rede.begin(), id_rede.end(), id_DTC_aux);
			pos=distance(id_rede.begin(), it_str);
			it_sub=lis_rede.begin();
			advance(it_sub,pos);
			
			dtc_aux=(*it_sub).get_dtc_rede();
			
			ids_ebs=(*it_sub).get_id_eb();
			it_str=find(ids_ebs.begin(), ids_ebs.end(), id_EB_aux);
			pos=distance(ids_ebs.begin(), it_str);
			
			eb_aux=(*it_sub).get_EB(pos);
			dtc_eb_aux=(*it_sub).get_DTC_EB(pos);
			
			num_EBs=(*it_sub).get_num_EBs();
			
			prop_aux=comb.get_prop_os(dtc_aux, eb_aux, dtc_eb_aux, num_EBs, ds, n_os);
			(*it_os).set_prop(prop_aux);
		}
	}
};

string Estrutura::verificar_val_OS(string linha, int linha_pos)
{	
	Acentos ace;
	string auxString, esp_branco, str_error;
	stringstream lineStream;
	int count=1, pos_sub;
	bool valido;
	OS aux_OS;
	list<string> ids_ebs;
	list<string>::iterator it_str;
	list<Rede>::iterator it_sub;
	list<string> dias_semana = {"domingo", "segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado"};
	double tempo;
	char * pEnd=NULL;
	
	//Transforma a string numa stream para obter as informações da linha mais facilmente
	lineStream.str(linha);
	
	//Identifica os valores das caracteristicas existentes
	while (getline(lineStream, auxString,';'))
	{
		//Apaga os espaços em branco para verificar se a string obtida é ""
		esp_branco=ace.espacos_branco(auxString);
		
		//Se houver mais características na linha do que aquelas definidas, a linha não é válida
		if (valido)
		{
			str_error = "A estrutura do ficheiro para a leitura das OSs não está bem definida na linha " + to_string(linha_pos);
			return str_error;
		}
		//Se auxString="", então a linha não é valida
		else if (esp_branco=="")
		{
			str_error = "Na linha "+to_string(linha_pos)+" existe uma característica sem valor!";
			return str_error;
		}
		else if (count==1)
		{
			aux_OS.set_nome_OS(auxString);
		}
		else if (count==2)
		{
			it_str=find(id_rede.begin(), id_rede.end(), auxString);
			if (it_str==id_rede.end())
			{
				str_error = "Não existe nenhum DTC com o ID introduzido na linha "+to_string(linha_pos);
				return str_error;
			}
			pos_sub=distance(id_rede.begin(), it_str);
			it_sub=lis_rede.begin();
			advance(it_sub,pos_sub);
			ids_ebs=(*it_sub).get_id_eb();
			aux_OS.set_id_DTC(auxString);
		}
		else if (count==3)
		{
			it_str=find(ids_ebs.begin(), ids_ebs.end(), auxString);
			if (it_str==ids_ebs.end())
			{
				str_error = "Não existe nenhuma EB com o ID introduzido na linha "+to_string(linha_pos);
				return str_error;
			}
			aux_OS.set_id_EB(auxString);
		}
		else if (count==4)
		{
			it_str=find(dias_semana.begin(), dias_semana.end(), auxString);
			if (it_str==dias_semana.end())
			{
				str_error = "Na linha "+to_string(linha_pos)+", o dia da semana introduzidao não é válido!";
				return str_error;
			}
			aux_OS.set_dia_semana(auxString);
		}
		else
		{
			tempo=strtod(auxString.c_str(), &pEnd);
			if (*pEnd)
			{
				str_error = "Na linha "+to_string(linha_pos)+", o tempo introduzido não é do tipo double!";
				return str_error;
			}
			if (tempo<0 || tempo>=24)
			{
				str_error = "O tempo introduzido na linha " + to_string(linha_pos) + " não é válido! O tempo tem de ser um valor que esteja no intervalo [0;24[";
				return str_error;
			}
			aux_OS.set_h_realizar(tempo);
			valido=true;
		}
		count++;
	}
	//O programa verifica se a linha introduzida é válida
	if (!valido)
	{
		str_error = "A estrutura do ficheiro para a leitura das OSs não está bem definida na linha " + to_string(linha_pos);
		return str_error;
	}
	lis_os.push_back(aux_OS);
	return "";
};

int Estrutura::abrir_ficheiro_OS(string nome_ficheiro)
{
	lis_os.clear();
	Acentos ace;
	ifstream f_ent(nome_ficheiro);
	ofstream f_out;
	string aux_comb, erros_txt="", opc_str;
	bool carac=true, erros=false, erros_carac=false;
	int linha_pos=2, opc;
	char * pEnd=NULL;
	
	try
	{
		//Verifica se ficheiro existe
		if (!f_ent.is_open())
		{
			throw fich_exis();
		}
		//Cria o ficheiro 'log_erros_OS'
		f_out.open("log_erros_OS.txt");
		
		//Enquanto houver linhas para ler no ficheiro
		while (getline(f_ent, aux_comb, '\n'))
		{
			//Verificar se tem caracteres ilegais
			aux_comb=ace.car_esp(aux_comb);
			//Se carac=true, verifica as caracteristicas
			if (carac)
			{
				if (aux_comb!="nome_OS;ID;ID_EB;dia_semana;h_realizar")
				{
					throw fich_vali_car_dif();
				}
				carac=false;
			}
			//Se carac=false, verifica os valores de cada DTC introduzido pelo utilizador
			else
			{
				erros_txt=verificar_val_OS(aux_comb, linha_pos);
				if (!erros_txt.empty())
				{
					erros=true;
					f_out << erros_txt << endl << endl;
				}
				linha_pos++;
			}
		}
		f_ent.close();
		f_out.close();
	}
	catch (fich_exis)
	{
		cout << "O ficheiro para a leitura das OSs " << ace.texto("não existe!") << endl;
		return 3;
	}
	catch (fich_vali_car_dif)
	{
		f_ent.close();
		f_out.close();
		cout << "As caracter" << ace.texto("í") << "sticas das OSs " << ace.texto("não estão") << " bem definidas!" << endl;
		erros_carac=true;
	}
	//Se houver erros no ficheiro em relação às características, o utilizador tem as seguintes opções:
	//opc=1 -> Introduzir um novo ficheiro de OSs;
	//opc=2 -> Desconsiderar a utilização das OSs;
	if (erros_carac)
	{
		opc=0;
		while (opc<1 || opc>2)
		{
			cout << endl << "Existe erros nas " << ace.texto("características") << " no ficheiro introduzido. Pretende:" << endl << "1.Introduzir um novo ficheiro das OSs;" << endl << ace.texto("2.Não considerar as OSs na simulação") << endl << endl;
			getline(cin,opc_str);
			opc=strtol(opc_str.c_str(),&pEnd,10);
			if (*pEnd)
			{
				cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
				opc=0;
			}
			else if (opc<1 || opc>2)
			{
				cout << endl << ace.texto("Opção inválida!") << endl << endl;
			}
		}
		opc+=2;
	}
	else if (lis_os.empty())
	{
		opc=0;
		while (opc<1 || opc>2)
		{
			if (erros)
			{
				cout << endl << ace.texto("Existem erros no ficheiro introduzido! Devido a esses erros não ");
			}
			else
			{
				cout << endl << ace.texto("Não ");
			}
			cout << ace.texto("foram inseridas nenhuma OS à simulação! Pretende:") << endl << "1.Introduzir um novo ficheiro das OSs;" << endl << ace.texto("2.Não considerar as OSs na simulação") << endl << endl;
			getline(cin,opc_str);
			opc=strtol(opc_str.c_str(),&pEnd,10);
			if (*pEnd)
			{
				cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
				opc=0;
			}
			else if (opc<1 || opc>2)
			{
				cout << endl << ace.texto("Opção inválida!") << endl << endl;
			}
		}
		opc+=2;
	}
	//Se houver erros no ficheiro em relação às OSs introduzidas, o utilizador tem as seguintes opções:
	//opc=1 -> Verificar o ficheiro dos erros
	//opc=2 -> Continuar com o programa
	//opc=3 -> Introduzir um novo ficheiro de OSs;
	//opc=4 -> Desconsiderar a utilização das OSs;
	else if (erros)
	{
		opc=0;
		while (opc<1 || opc>4)
		{
			cout << endl << "Existe erros no ficheiro introduzido. Pretende:" << endl << "1.Verificar o ficheiro dos erros;" << endl << "2.Continuar com o programa;" << endl << "3.Introduzir um novo ficheiro das OSs;" << endl << ace.texto("4.Desconsiderar a utilização das OSs;") << endl << endl;
			getline(cin,opc_str);
			opc=strtol(opc_str.c_str(),&pEnd,10);
			if (*pEnd)
			{
				cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
				opc=0;
			}
			else if (opc<1 || opc>4)
			{
				cout << endl << ace.texto("Opção inválida!") << endl << endl;
			}
		}
		
		//Abrir o ficheiro dos erros criado durante a leitura do ficheiro
		if (opc==1)
		{
			f_ent.open("log_erros_OS.txt");
			while (getline(f_ent, aux_comb, '\n'))
			{
				cout << ace.texto(aux_comb) << endl;
			}
			f_ent.close();
			opc=0;
			
			//Depois de abrir o ficheiro, o utilizador tem de escolher se pretende continuar com o programa, inserir um novo ficheiro de OSs ou desconsiderar a utilização das OSs
			while (opc<1 || opc>3)
			{
				cout << "Pretende:" << endl << "1.Continuar com o programa;" << endl << "2.Introduzir um novo ficheiro das OSs;" << endl << ace.texto("3.Desconsiderar a utilização das OSs;") << endl << endl;
				getline(cin,opc_str);
				opc=strtol(opc_str.c_str(),&pEnd,10);
				if (*pEnd)
				{
					cout << endl << ace.texto("É necessário introduzir um valor inteiro!") << endl << endl;
					opc=0;
				}
				else if (opc<1 || opc>3)
				{
					cout << endl << ace.texto("Opção inválida!") << endl << endl;
				}
			}
			opc++;
		}
		
		//Antes de introduzir um novo ficheiro, é necessário apagar todas os valores introduzidos com o ficheiro atual
		if (opc==3 || opc==4)
		{
			lis_os.clear();
		}
	}
	//Se não houver erros, continua-se com o programa
	else
	{
		opc=2;
	}
	//Utiliza-se o método Bubble Sort para ordenar as OS por horas
	if (opc==2)
	{
		bool ordenado=false;
		list<OS>::iterator it_os;
		while (!ordenado)
		{
			ordenado=true;
			for (it_os=lis_os.begin(); it_os!=prev(lis_os.end()); it_os++)
			{
				if ((*it_os).get_h_realizar()>(*next(it_os)).get_h_realizar())
				{
					swap(*it_os,*next(it_os));
					ordenado=false;
				}
			}
		}
	}
	return opc;
};

struct tm Estrutura::dia_seguinte(struct tm timeInfo)
{
	timeInfo.tm_mday+=1;
	if (timeInfo.tm_mday==29 && timeInfo.tm_mon==1 && timeInfo.tm_year%4!=0)
	{
		timeInfo.tm_mon+=1;
		timeInfo.tm_mday=1;
	}
	else if (timeInfo.tm_mday==30 && timeInfo.tm_mon==1)
	{
		timeInfo.tm_mon+=1;
		timeInfo.tm_mday=1;
	}
	else if (timeInfo.tm_mday==31 && (timeInfo.tm_mon==3 || timeInfo.tm_mon==5 || timeInfo.tm_mon==8 || timeInfo.tm_mon==10))
	{
		timeInfo.tm_mon+=1;
		timeInfo.tm_mday=1;
	}
	else if (timeInfo.tm_mday==32)
	{
		timeInfo.tm_mon=(timeInfo.tm_mon+1)%12;
		if (timeInfo.tm_mon==0)
		{
			timeInfo.tm_year+=1;
		}
		timeInfo.tm_mday=1;
	}
	timeInfo.tm_wday=(timeInfo.tm_wday+1)%7;
	return timeInfo;
}

int Estrutura::numero_dias(int dia1, int dia2)
{
	int num_dias=dia1-dia2;
	if (num_dias<0)
	{
		num_dias+=7;
	}
	return num_dias;
};

double Estrutura::gerar_valor() //gera um valor entre 0 e 1
{
    return (double)rand() / (double)RAND_MAX ;
};

string Estrutura::sucesso_insucesso(double prop, int n_dias, double val_zero_um)
{
	//Se o valor da proporção de sucesso for maior ou igual do que aquele gerado, então houve sucesso no envio da leitura
	if (prop>=val_zero_um)
	{
		return to_string(n_dias*24);
	}
	return "";
}

list<list<string>> Estrutura::escrever_ficheiro(struct tm timeInfo, Rede aux_sub, string id, list<list<string>> ebs_falhadas)
{
	int dia_lei_falh, n_dias;
	double pr;
	string aux_id, aux_id_falhado, aux_val_suc, aux_data, aux_id_eb_falh;
	DTC dtc_asso;
	list<EB> EBs_asso;
	list<DTC_EB> DTC_EBs_asso;
	list<list<double>> prop_asso;
	list<string> id_eb_asso, enviar, valores;
	list<list<string>> aux_falhados;
	list<list<string>>::iterator it_lis_str;
	list<EB>::iterator it_eb;
	list<DTC_EB>::iterator it_dtc_eb;
	list<string>::iterator it_str, it_id_eb;
	list<list<double>>::iterator it_prop;
	list<double>::iterator it_val_prop;
	
	aux_id=id+";";
	dtc_asso = aux_sub.get_dtc_rede();
	EBs_asso = aux_sub.get_lista_eb();
	DTC_EBs_asso = aux_sub.get_lista_dtc_eb();
	prop_asso = aux_sub.get_lista_prop();
	id_eb_asso = aux_sub.get_id_eb();
	
	//O programa vai tentar obter as leituras falhadas de dias anteriores do DTC considerado antes de verificar as do dia atual
	for (it_lis_str=ebs_falhadas.begin(); it_lis_str!=ebs_falhadas.end(); it_lis_str++)
	{
		//ID do DTC da EB que não enviou a leitura
		aux_id_falhado=*prev(prev(prev(prev((*it_lis_str).end()))));
		
		//Se o ID coincidir, simular se houve sucesso de envio da leitura
		if(aux_id==aux_id_falhado)
		{
			//Valor da proporção de sucesso
			pr = stod(*(prev((*it_lis_str).end())));
			
			//Dia da suposta leitura
			dia_lei_falh=stoi(*prev(prev((*it_lis_str).end())));
			
			//ID da EB
			aux_id_eb_falh=*prev(prev(prev((*it_lis_str).end())));
			
			//Número de dias que não se obtém a leitura
			n_dias=numero_dias(timeInfo.tm_wday, dia_lei_falh);
			
			//Simular se houve sucesso
			aux_val_suc=sucesso_insucesso(pr, n_dias, gerar_valor());
			
			//Se houve sucesso, a informação do DTC/EB passa para uma list<string> que posteriormente será enviado para uma list<list<string>> com toda a informação a escrever no ficheiro
			if (aux_val_suc!="")
			{
				enviar=*it_lis_str;
				enviar.pop_back();
				enviar.pop_back();
				enviar.pop_back();
				enviar.pop_back();
				enviar.pop_back();
				enviar.push_back(aux_val_suc);
				enviar.push_back(aux_id);
				enviar.push_back(aux_id_eb_falh);
				esc_fich.push_back(enviar);
				enviar.clear();
			}
			//Caso contrário, a informação será novamente introduzida na list<string> ebs_falhadas
			else
			{
				aux_falhados.push_back(*it_lis_str);
			}
		}
		else
		{
			aux_falhados.push_back(*it_lis_str);
		}
	}
	ebs_falhadas=aux_falhados;
	
	//Simular o sucesso de envio para todas as EBs do DTC associado
	it_eb = EBs_asso.begin();
	it_dtc_eb = DTC_EBs_asso.begin();
	it_id_eb=id_eb_asso.begin();
	it_prop = prop_asso.begin();
	
	while (it_eb!=EBs_asso.end())
	{
		//Dia da simulação
		aux_data=to_string(timeInfo.tm_mday);
		//Mês da simulação
		switch(timeInfo.tm_mon)
		{
			case 0:
				aux_data+="JAN";
				break;
			case 1:
				aux_data+="FEB";
				break;
			case 2:
				aux_data+="MAR";
				break;
			case 3:
				aux_data+="APR";
				break;
			case 4:
				aux_data+="MAY";
				break;
			case 5:
				aux_data+="JUN";
				break;
			case 6:
				aux_data+="JUL";
				break;
			case 7:
				aux_data+="AUG";
				break;
			case 8:
				aux_data+="SEP";
				break;
			case 9:
				aux_data+="OCT";
				break;
			case 10:
				aux_data+="NOV";
				break;
			default:
				aux_data+="DEC";
		}
		//Ano da simulação
		aux_data+=to_string(timeInfo.tm_year);
		enviar.push_back(aux_data+";");

		//Características do DTC
		valores=dtc_asso.get_valor();
		for (it_str=valores.begin(); it_str!=valores.end(); it_str++)
		{
			enviar.push_back(*it_str+";");
		}
		
		//Características da EB
		valores=(*it_eb).get_valor();
		for (it_str=valores.begin(); it_str!=valores.end(); it_str++)
		{
			enviar.push_back(*it_str+";");
		}
		
		//Características do DTC+EB
		valores=(*it_dtc_eb).get_valor();
		for (it_str=valores.begin(); it_str!=valores.end(); it_str++)
		{
			enviar.push_back(*it_str+";");
		}
		
		//Número de EBs no DTC
		enviar.push_back(to_string(EBs_asso.size())+";");
		
		//Obter a probabilidade e verificar se houve sucesso ou insucesso
		it_val_prop=(*it_prop).begin();
		advance(it_val_prop,timeInfo.tm_wday);
		aux_val_suc=sucesso_insucesso(*it_val_prop,0,gerar_valor());
		enviar.push_back(aux_val_suc);
		
		//ID do DTC e da EB
		enviar.push_back(aux_id);
		enviar.push_back(*it_id_eb+";");
		
		//Se não houve sucesso, introduzir na variável "ebs_falhadas" onde será introduzido o dia da semana e o valor da propoção
		if (aux_val_suc=="")
		{
			enviar.push_back(to_string(timeInfo.tm_wday));
			enviar.push_back(to_string(*it_val_prop));
			ebs_falhadas.push_back(enviar);
		}
		else
		{
			esc_fich.push_back(enviar);
		}
		//Passar para o próxima EB
		enviar.clear();
		advance(it_eb,1);
		advance(it_dtc_eb,1);
		advance(it_id_eb,1);
		advance(it_prop,1);
	}
	return ebs_falhadas;
};

void Estrutura::simular_leituras(string nome_ficheiro)
{
	//O programa vai simular os resultados dos sete dias seguintes à data atual
	auto tempo_hoje = chrono::system_clock::now();
	time_t tempo_hoje_t = chrono::system_clock::to_time_t(tempo_hoje);
	struct tm timeInfo;
	timeInfo = *localtime(&tempo_hoje_t);
	timeInfo.tm_year+=1900;
	timeInfo=dia_seguinte(timeInfo);
	
	ofstream f_out;
	Rede aux_rede;
	list<string> aux_lis;
	list<string>::iterator it_str;
	list<list<string>> ebs_falhadas;
	list<Rede>::iterator it_sub;
	string aux;
	int i;
	list<list<string>>::iterator it_lis_str;
	
	f_out.open(nome_ficheiro);
	
	//1ª linha -> Nome das características
	//As características são adicionadas da seguinte maneira:
	//1º: Data de Leitura;
	f_out << "data_leit;";
	
	//2º: Características DTC
	aux_rede=lis_rede.front();
	aux_lis=aux_rede.get_car_DTC();
	for (it_str=aux_lis.begin(); it_str!=aux_lis.end(); it_str++)
	{
		f_out << *it_str << ";";
	}
	
	//3º: Características EB
	aux_lis=aux_rede.get_car_EB();
	for (it_str=aux_lis.begin(); it_str!=aux_lis.end(); it_str++)
	{
		f_out << *it_str << ";";
	}
	
	//4º: Características DTC+EB
	aux_lis=aux_rede.get_car_DTC_EB();
	for (it_str=aux_lis.begin(); it_str!=aux_lis.end(); it_str++)
	{
		f_out << *it_str << ";";
	}
	
	//5º: Quantidade de EBs no DTC, Envio da leitura ao fim de 1 dia e se foi obtida, tempo que demorou a enviar a leitura, ID do DTC
	f_out << "qtd_EBs_regi;fim_1dia;leitura_obtida;tempo_leit_EB;ID;ID_EB" << endl;
	
	esc_fich.clear();
	
	//Início da simulação
	for (i=0; i<7; i++)
	{
		//Início da contagem do tempo
		auto start = chrono::high_resolution_clock::now();
		it_str=id_rede.begin();
	
		//Verifica para cada DTC quais EBs tiveram sucesso em mandar as suas leituras
		for (it_sub=lis_rede.begin(); it_sub!=lis_rede.end(); it_sub++)
		{
			ebs_falhadas=escrever_ficheiro(timeInfo, *it_sub, *it_str, ebs_falhadas);
			advance(it_str,1);
		}
		
		//Depois de verificar as EBs que enviaram a leitura com sucesso, o programa vai escrever num ficheiro as ebs que enviaram as suas leituras
		for (it_lis_str=esc_fich.begin(); it_lis_str!=esc_fich.end(); it_lis_str++)
		{
			//Final da contagem do tempo
			auto finish = chrono::high_resolution_clock::now();
			chrono::duration<double> elapsed = finish - start;
			
			//Para cada EB, o programa vai escrever coluna a coluna os resultados obtidos
			for (it_str=(*it_lis_str).begin(); it_str!=(*it_lis_str).end(); it_str++)
			{
				//A antepenúltima coluna tem o número de dias que a EB não conseguiu enviar a sua leitura de um certo dia
				if (distance(it_str,(*it_lis_str).end())==3)
				{
					int num_dias = stoi(*it_str)/24;
					//Se o número de dias for 0, então a leitura foi recebida no 1º dia, logo fim_1_dia=1 e fim_5_dia=1
					if (num_dias==0)
					{
						f_out << "1;1;";
					}
					//Caso contrário, fim_1_dia=0 e fim_5_dia=0
					else
					{
						f_out << "0;1;";
					}
					//Tempo total que leitura demorou até chegar à BD
					f_out << to_string(stod(*it_str)+elapsed.count()) << ";";
				}
				else if (distance(it_str,(*it_lis_str).end())==1)
				{
					aux=*it_str;
					aux.pop_back();
					f_out << aux;
				}
				//Todas as outras colunas escreve-se apenas o valor
				else
				{
					f_out << *it_str;
				}
			}
			f_out << endl;
		}
		//Apaga-se o ficheiro de escrita após introduzir todos as leituras recebidas para o ficheiro
		esc_fich.clear();
		
		//Para para o dia seguinte
		timeInfo=dia_seguinte(timeInfo);
	}
	//No final, se houver ebs falhadas, o programa irá escreve-las sem apresentar um tempo final (pois nunca chegou a leitura)
	for (it_lis_str=ebs_falhadas.begin(); it_lis_str!=ebs_falhadas.end(); it_lis_str++)
	{
		aux_lis=*it_lis_str;
		aux_lis.pop_back();
		aux_lis.pop_back();
		for (it_str=aux_lis.begin(); it_str!=aux_lis.end(); it_str++)
		{
			if (distance(it_str,aux_lis.end())==3)
			{
				f_out << "0;0;;";
			}
			else if (distance(it_str,aux_lis.end())==1)
			{
				aux=*it_str;
				aux.pop_back();
				f_out << aux;
			}
			else
			{
				f_out << *it_str;
			}
		}
		f_out << endl;
		aux_lis.clear();
	}
	f_out.close();
};

void Estrutura::simular_OS(string nome_ficheiro)
{
	//O programa vai simular os resultados dos sete dias seguintes à data atual
	auto tempo_hoje = chrono::system_clock::now();
	time_t tempo_hoje_t = chrono::system_clock::to_time_t(tempo_hoje);
	struct tm timeInfo;
	timeInfo = *localtime(&tempo_hoje_t);
	timeInfo.tm_year+=1900;
	timeInfo=dia_seguinte(timeInfo);
	
	ofstream f_out;
	Rede aux_rede;
	DTC dtc_aux;
	list<string> aux_lis;
	list<string>::iterator it_str;
	list<OS>::iterator it_os;
	list<Rede>::iterator it_sub;
	list<EB>::iterator it_lis_eb;
	list<DTC_EB>::iterator it_lis_dtc_eb;
	list<string> dias_semana = {"domingo", "segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado"};
	string ds_os, aux_data, n_os, aux_id_DTC, aux_id_EB, aux_val_suc;
	int i, int_ds_os, pos;
	double pr, hr;
	
	f_out.open(nome_ficheiro);
	
	//1ª linha -> Nome das características
	//As características são adicionadas da seguinte maneira:
	//1º: Data de Leitura, nome da OS;
	f_out << "data_OS;nome_OS;";
	
	//2º: Características DTC
	aux_rede=lis_rede.front();
	aux_lis=aux_rede.get_car_DTC();
	for (it_str=aux_lis.begin(); it_str!=aux_lis.end(); it_str++)
	{
		f_out << *it_str << ";";
	}
	
	//3º: Características EB
	aux_lis=aux_rede.get_car_EB();
	for (it_str=aux_lis.begin(); it_str!=aux_lis.end(); it_str++)
	{
		f_out << *it_str << ";";
	}
	
	//4º: Características DTC+EB
	aux_lis=aux_rede.get_car_DTC_EB();
	for (it_str=aux_lis.begin(); it_str!=aux_lis.end(); it_str++)
	{
		f_out << *it_str << ";";
	}
	
	//5º: Quantidade de EBs no DTC, sucesso da OS, tempo que demorou a enviar a leitura, ID do DTC
	f_out << "qtd_EBs_regi;sucesso;tempo_OS;ID;ID_EB" << endl;
	
	esc_fich.clear();
	
	//Início da simulação
	for (i=0; i<7; i++)
	{
		
		//Início da contagem do tempo
		auto start = chrono::high_resolution_clock::now();
	
		//Verifica para cada OS quais é que tiveram sucesso
		it_os=lis_os.begin();
		while (it_os!=lis_os.end())
		{
			ds_os=(*it_os).get_dia_semana();
			it_str=find(dias_semana.begin(), dias_semana.end(), ds_os);
			int_ds_os=distance(dias_semana.begin(), it_str);
			if (int_ds_os==timeInfo.tm_wday)
			{
				//Dia da simulação
				aux_data=to_string(timeInfo.tm_mday)+"/";
				//Mês da simulação
				aux_data+=to_string(timeInfo.tm_mon+1)+"/";
				//Ano da simulação
				aux_data+=to_string(timeInfo.tm_year)+";";
				f_out << aux_data;
				
				//Nome da OS
				n_os=(*it_os).get_nome_OS();
				f_out << n_os << ";";
				
				//Buscar valores dos DTC, EB, DTC+EB
				//DTC
				aux_id_DTC=(*it_os).get_id_DTC();
				it_str=find(id_rede.begin(), id_rede.end(), aux_id_DTC);
				pos=distance(id_rede.begin(), it_str);
				it_sub=lis_rede.begin();
				advance(it_sub,pos);
				aux_rede=*it_sub;
				dtc_aux=aux_rede.get_dtc_rede();
				aux_lis=dtc_aux.get_valor();
				for(it_str=aux_lis.begin(); it_str!=aux_lis.end(); it_str++)
				{
					f_out << *it_str << ";";
				}
				
				//EB, DTC+EB
				aux_id_EB=(*it_os).get_id_EB();
				aux_lis=aux_rede.get_id_eb();
				it_str=find(aux_lis.begin(), aux_lis.end(), aux_id_EB);
				pos=distance(aux_lis.begin(), it_str);
				it_lis_eb=aux_rede.get_lista_eb().begin();
				advance(it_lis_eb, pos);
				aux_lis=(*it_lis_eb).get_valor();
				for(it_str=aux_lis.begin(); it_str!=aux_lis.end(); it_str++)
				{
					f_out << *it_str << ";";
				}
				it_lis_dtc_eb=aux_rede.get_lista_dtc_eb().begin();
				advance(it_lis_dtc_eb, pos);
				aux_lis=(*it_lis_dtc_eb).get_valor();
				for(it_str=aux_lis.begin(); it_str!=aux_lis.end(); it_str++)
				{
					f_out << *it_str << ";";
				}
				
				//Número de EBs
				f_out << to_string(aux_rede.get_num_EBs()) << ";";
				
				//Verificar se a OS teve sucesso
				pr=(*it_os).get_prop();
				aux_val_suc=sucesso_insucesso(pr, 0, gerar_valor());
				if (aux_val_suc!="")
				{
					f_out << "1;";
				}
				else
				{
					f_out << "0;";
				}
				//Adicionar Tempo
				//Final da contagem do tempo
				auto finish = chrono::high_resolution_clock::now();
				chrono::duration<double> elapsed = finish - start;
				
				hr=(*it_os).get_h_realizar();
				f_out << to_string(elapsed.count()) << ";";
				//Adicionar ID DTC
				f_out << aux_id_DTC << ";";
				//Adicionar ID EB
				f_out << aux_id_EB << endl;
				
				//apagar a OS tratada
				it_os=lis_os.erase(it_os);
			}
			else
			{
				it_os++;
			}
		}
		//Para para o dia seguinte
		timeInfo=dia_seguinte(timeInfo);
	}
	f_out.close();
};
