#include <iostream>
#include <string>
#include <list>
#include <fstream>
#include <algorithm>
#include <sstream>
#include "Acentos.cpp"
#include "Caracteristica.cpp"
#include "DTC.hpp"
#include "EB.hpp"
#include "DTC_EB.hpp"
#include "Combinacao.hpp"
#include "Combinacoes.hpp"

using namespace std;

Combinacao Combinacoes::verificar_car(string linha, bool fich_OS, list<string> dtc_car, list<string> eb_car, list<string> dtc_eb_car)
{		
	Acentos ace;
	stringstream lineStream;
	string auxString, esp_brancos;
	Combinacao combAux;
	int car_tipo=1;
	bool valido;
	
	//Transforma a string numa stream para obter as informa��es da linha mais facilmente
	lineStream.str(linha);
	
	//Identifica quais as caracteristicas existentes e o tipo
	while (getline(lineStream,auxString,';'))
	{
		//Apaga os espa�os em branco para verificar se a string obtida � ""
		esp_brancos=ace.espacos_branco(auxString);
		
		//Se a string estiver vazia, significa que n�o existe mais nenhuma caracteristica associada ao tipo a analisar, e passa para o pr�ximo tipo.
		if (esp_brancos.empty())
		{
			car_tipo++;
		}
		//car_tipo=1 -> Caracteristica do DTC
		else if (car_tipo==1)
		{
			combAux.add_car_DTC(auxString);
		}
		//car_tipo=2 -> Caracteristica da EB
		else if (car_tipo==2)
		{
			combAux.add_car_EB(auxString);
		}
		//car_tipo=3 -> Caracteristica EB/DTC
		else if (car_tipo==3)
		{
			combAux.add_car_DTC_EB(auxString);
		}
		//car_tipo=4 -> Caracteristica global (Ex: dia da semana, nome da OS, etc.)
		else if (car_tipo==4)
		{
			//Se a caracteristica "dia_semana" n�o existir, o ficheiro n�o � v�lido
			if (auxString.compare("dia_semana")!=0)
			{
				throw fich_vali_car();
			}
			getline(lineStream,auxString, ';');
			//Verifica se a caracteristica "nome_OS" existe. Se existir, o ficheiro das combina��es a analisar tem de ser relativo �s OS. Caso contr�rio, o ficheiro n�o � v�lido.
			//Da mesma maneira, se "nome_OS" n�o existir e o ficheiro das combina��es a analisar for relativo �s leituras, o ficheiro tamb�m n�o ser� v�lido.
			if (auxString.compare("nome_OS")==0)
			{
				if (!fich_OS)
				{
					throw fich_vali_nao_OS();
				}
				getline(lineStream,auxString,';');
			}
			else if (fich_OS)
			{
				throw fich_vali_OS();
			}
			//Se "qtd_EBs_regi_divisoes" existir, o programa ter� em conta o n�mero de EBs existentes em cada DTC introduzido pelo utilizador
			if (auxString.compare("qtd_EBs_regi_divisoes")==0)
			{
				qtd_EBs=true;
				getline(lineStream, auxString,';');
			}
			else
			{
				qtd_EBs=false;
			}
			getline(lineStream, auxString,';');
			//Verifica se existe as caracteristicas "IC_inf" e "IC_sup". Se uma delas n�o existir, o ficheiro n�o � v�lido
			if (auxString.compare("IC_inf")!=0)
			{
				throw fich_vali_car();
			}
			getline(lineStream, auxString,';');
			if (auxString.compare("IC_sup")!=0)
			{
				throw fich_vali_car();
			}
			getline(lineStream,auxString, '\n');
			
			//Se chegar a este ponto, as caracteristicas introduzidas s�o v�lidas
			valido=true;
		}
		else
		{
			throw fich_vali_car();
		}
	}
	//O programa verifica se o ficheiro introduzido � v�lido
	if (valido)
	{
		//Se for v�lido e se o ficheiro estiver relacionado com as OS, o programa verifica se as caracteristicas introduzidas s�o iguais das caracteristicas das leituras. Se houver diferen�as, o ficheiro das OS n�o � v�lido
		if (fich_OS)
		{
			if (!(combAux.get_car_DTC()==dtc_car) || !(combAux.get_car_EB()==eb_car) || !(combAux.get_car_DTC_EB()==dtc_eb_car))
			{
				throw fich_vali_OS_car();
			}
		}
		return combAux;
	}
	else
	{
		throw fich_vali_car();
	}
}

bool Combinacoes::verificar_dia_semana(string auxString)
{
	list<string> dias_semana = {"domingo", "segunda-feira", "ter�a-feira", "quarta-feira", "quinta-feira", "sexta-feira", "s�bado"};
	list<string>::iterator it;
	it=find(dias_semana.begin(), dias_semana.end(), auxString);
	if (it==dias_semana.end())
	{
		return false;
	}
	return true;
}

double Combinacoes::gerar_val_prop(double ic_inf, double ic_sup, double val_zero_um)
{
	double val_dev=val_zero_um*(ic_sup-ic_inf)+ic_inf;
	return val_dev;
};

double Combinacoes::gerar_valor()
{
    return (double)rand() / (double)RAND_MAX ;
};

string Combinacoes::verificar_val(string linha, Combinacao combAux, int linha_pos, bool fich_OS)
{
	Acentos ace;
	stringstream lineStream;
	string auxString, str_error="", esp_brancos;
	int car_tipo=1, pos_car, min_eb, max_eb, pos_val=0, num_car_DTC=combAux.get_num_car_DTC(), num_car_EB=combAux.get_num_car_EB(), num_car_DTC_EB=combAux.get_num_car_DTC_EB();
	double ic_inf, ic_sup;
	bool valido=false;
	char * pEnd = NULL;	
	
	//Transforma a string numa stream para obter as informa��es da linha mais facilmente
	lineStream.str(linha);
	
	//Identifica os valores das caracteristicas existentes
	while (getline(lineStream,auxString,';'))
	{
		//Apaga os espa�os em branco para verificar se a string obtida � ""
		esp_brancos=ace.espacos_branco(auxString);
		
		//Se a string estiver vazia, significa que n�o existe mais nenhuma caracteristica associada ao tipo a analisar, e passa para o pr�ximo tipo.
		if (esp_brancos.empty())
		{
			car_tipo++;
			pos_val=0;
		}
		//car_tipo=1 -> Valor de uma das caracteristicas do DTC
		else if (car_tipo==1)
		{
			if (num_car_DTC>pos_val)
			{
				combAux.add_val_DTC(auxString, pos_val);
				pos_val++;
			}
			else
			{
				str_error="O n�mero de caracter�sticas definidas na linha "+to_string(linha_pos)+" � diferente daquelas que deveriam existir!";
				return str_error;
			}
		}
		//car_tipo=2 -> Valor de uma das caracteristicas do EB
		else if (car_tipo==2)
		{
			if (num_car_EB>pos_val)
			{
				combAux.add_val_EB(auxString, pos_val);
				pos_val++;
			}
			else
			{
				str_error="O n�mero de caracter�sticas definidas na linha "+to_string(linha_pos)+" � diferente daquelas que deveriam existir!";
				return str_error;
			}
		}
		//car_tipo=3-> Valor de uma das caracteristicas EB/DTC
		else if (car_tipo==3)
		{
			if (num_car_DTC_EB>pos_val)
			{
				combAux.add_val_DTC_EB(auxString, pos_val);
				pos_val++;
			}
			else
			{
				str_error="O n�mero de caracter�sticas definidas na linha "+to_string(linha_pos)+" � diferente daquelas que deveriam existir!";
				return str_error;
			}
		}
		//car_tipo=4-> Valor de uma das caracteristicas globais (Ex: dia da semana, nome da OS, etc.)
		else if (car_tipo==4)
		{
			//O programa verifica se o dia da semana � v�lido. Se n�o for v�lido, o programa coloca no ficheiro log uma mensagem de erro a dizer que na linha em quest�o, o dia da semana n�o � v�lido
			if (!verificar_dia_semana(auxString))
			{
				str_error= "O dia da semana na linha "+to_string(linha_pos)+" do ficheiro .csv n�o � v�lido!";
				return str_error;
			}
			combAux.set_dia_semana(auxString);
			getline(lineStream, auxString, ';');
			//Se o ficheiro for das OS, o programa vai verificar se foi introduzido um nome � OS realizada na combina��o. Se n�o for, o programa coloca no ficheiro log uma mensagem de erro a dizer que na linha em quest�o, nao existe um nome da OS
			if (fich_OS)
			{
				if (auxString=="")
				{
					str_error= "N�o foi adicionada um nome � OS na linha "+to_string(linha_pos);
					return str_error;
				}
				combAux.set_nome_OS(auxString);
				getline(lineStream, auxString, ';');
			}
			else
			{
				combAux.set_nome_OS("");
			}
			//Se as combina��es tiverem o campo "qtd_EBs_regi_divisoes", o programa vai verificar se a string correspondente a este campo segue os seguintes passos:
			//1�: A string tem o caracter "-"
			//2�: A string tem um n�mero inteiro antes e depois do caracter "-" apenas
			//3�: O n�mero inteiro antes do caracter "-" � menor que o n�mero depois do caracter "-"
			//Se algum destes pontos falhar, o programa escreve no log uma mensagem de erro, especificando qual o erro que encontrou
			if (qtd_EBs)
			{		
				pos_car=auxString.find('-');
				if (pos_car==-1)
				{
					str_error= "A quantidade de EBs no DTC na linha "+to_string(linha_pos)+" do ficheiro .csv n�o � v�lido! Falta o caracter '-'!";
					return str_error;
				}
				min_eb=strtol(auxString.substr(0,pos_car).c_str(),&pEnd,10);
				if (*pEnd)
				{
					str_error= "A quantidade de EBs no DTC na linha "+to_string(linha_pos)+" do ficheiro .csv n�o � v�lido! O valor antes do caracter '-' n�o tem formato de inteiro!";
					return str_error;
				}
				max_eb=strtol(auxString.substr(pos_car+1).c_str(),&pEnd,10);
				if (*pEnd)
				{
					str_error= "A quantidade de EBs no DTC na linha "+to_string(linha_pos)+" do ficheiro .csv n�o � v�lido! O valor depois do caracter '-' n�o tem formato de inteiro!";
					return str_error;
				}
				if (max_eb<min_eb)
				{
					str_error= "A quantidade de EBs no DTC na linha "+to_string(linha_pos)+" do ficheiro .csv n�o � v�lido! O valor m�nimo � superior ao valor m�nimo de EBs no DTC!";
					return str_error;
				}
				combAux.set_qtd_EBs(min_eb, max_eb);
				getline(lineStream, auxString, ';');
			}
			else
			{
				combAux.set_qtd_EBs(0,0);
			}
			getline(lineStream, auxString, ';');
			
			//Para calcular a propor��o de sucesso a utilizar(OS/Leituras), o programa precisa de verificar os valores correspondentes aos intervalos de confian�a. Os valores t�m que satisfazer as seguintes condi��es:
			//1�: Os valores t�m de ser do tipo double
			//2�: Os valores t�m de estar compreendidos entre 0 e 1
			//3�: O intervalo de confian�a inferior tem de ser menor ou igual que o intervalo de confian�a superior
			//Se algum destes pontos falhar, o programa escreve no log uma mensagem de erro, especificando qual o erro que encontrou
			ic_inf=strtod(auxString.c_str(), &pEnd);
			if (*pEnd)
			{
				str_error= "O valor inferior do intervalo de confian�a na linha "+to_string(linha_pos)+" do ficheiro .csv n�o � v�lido! O valor n�o tem formato de um valor decimal!";
				return str_error;
			}
			if (ic_inf>1 || ic_inf<0)
			{
				str_error= "O valor inferior do intervalo de confian�a na linha "+to_string(linha_pos)+" do ficheiro .csv n�o � v�lido! O valor n�o est� entre 0 e 1!";
				return str_error;
			}
			getline(lineStream,auxString, ';');
			ic_sup=strtod(auxString.c_str(), &pEnd);
			if (*pEnd)
			{
				str_error= "O valor superior do intervalo de confian�a na linha "+to_string(linha_pos)+" do ficheiro .csv n�o � v�lido! O valor n�o tem formato de um valor decimal!";
				return str_error;
			}
			if (ic_sup>1 || ic_sup<0)
			{
				str_error= "O valor superior do intervalo de confian�a na linha "+to_string(linha_pos)+" do ficheiro .csv n�o � v�lido! O valor n�o est� entre 0 e 1!";
				return str_error;
			}
			if (ic_sup<ic_inf)
			{
				str_error= "O valores dos intervalo de confian�a na linha "+to_string(linha_pos)+" do ficheiro .csv n�o s�o v�lidos! O valor inferior � maior do que o valor superior do intervalo de confian�a!";
				return str_error;
			}
			combAux.set_prop(gerar_val_prop(ic_inf,ic_sup,gerar_valor()));
			getline(lineStream,auxString, '\n');
			//Verifica se o n�mero de caracteristicas obtidas coincide com o n�mero de caracteristicas obtidas na primeira linha do ficheiro. Se n�o coincidir, escreve uma mensagem de erro no log
			if (!combAux.comparar_tamanhos_DTC() || !combAux.comparar_tamanhos_EB() || !combAux.comparar_tamanhos_DTC_EB())
			{
				str_error="O n�mero de caracter�sticas definidas na linha "+to_string(linha_pos)+" � diferente daquelas que deveriam existir!";
				return str_error;
			}
			//Se chegar a este ponto, os valores para as caracteristicas introduzidas s�o v�lidas
			valido=true;
		}
		//Se car_tipo>=5, a linha n�o � v�lida
		else
		{
			str_error="A estrutura do ficheiro n�o est� bem definida na linha "+to_string(linha_pos);
			return str_error;
		}
	}
	//O programa verifica se o ficheiro introduzido � v�lido
	if (!valido)
	{
		str_error="A estrutura do ficheiro n�o est� bem definida na linha "+to_string(linha_pos);
		return str_error;
	}
	lista_comb.push_back(combAux);
	return "";
};

bool Combinacoes::abrir_ficheiro(string nome_ficheiro, bool fich_OS, list<string> dtc_car, list<string> eb_car, list<string> dtc_eb_car)
{
	Acentos ace;
	ifstream f_ent(nome_ficheiro);
	ofstream f_out;
	string aux_comb, erros_txt, opc_str;
	Combinacao combAux;
	bool carac=true, erros=false;
	int linha_pos=2, opc;
	char * pEnd = NULL;
	
	try
	{
		//verifica se ficheiro existe
		if (!f_ent.is_open())
		{
			throw fich_exis();
		}
		//verifica se as combina��es a introduzir est�o relacionadas com as OS ou as leituras e cria o ficheiro 'log_erros' consoante as combina��es
		if (fich_OS)
		{
			f_out.open("log_erros_comb_OS.txt");
		}
		else
		{
			f_out.open("log_erros_comb_leit.txt");
		}
		//Enquanto houver linhas para ler no ficheiro
		while (getline(f_ent, aux_comb, '\n'))
		{	
			//Verificar se a string tem caracteres ilegais
			aux_comb=ace.car_esp(aux_comb);
			
			//Se carac=true, verifica as caracteristicas
			if (carac)
			{
				combAux=verificar_car(aux_comb, fich_OS, dtc_car, eb_car, dtc_eb_car);
				carac=false;
			}
			//Se carac=false, verifica os valores de cada combina��o
			else
			{
				erros_txt=verificar_val(aux_comb,combAux,linha_pos,fich_OS);
				if (!erros_txt.empty())
				{
					erros=true;
					f_out << erros_txt << endl << endl;
				}
				linha_pos++;
			}
		}
		//Encerrar os ficheiros de entrada e de sa�da
		f_ent.close();
		f_out.close();
		
		//Se houver erros no ficheiro o utilizador tem as seguintes op��es:
		//opc=1 -> Verificar o ficheiro dos erros
		//opc=2 -> Continuar com o programa
		//opc=3 -> Introduzir um novo ficheiro de combina��es (OS ou Leituras dependendo da op��o atual)
		if (erros)
		{
			opc=0;
			while (opc<1 || opc>3)
			{
				cout << endl << "Existe erros no ficheiro introduzido. Pretende:" << endl << "1.Verificar o ficheiro dos erros;" << endl << "2.Continuar com o programa;" << endl << "3.Introduzir outro ficheiro das combina" << ace.texto("��es") << endl << endl;
				getline(cin,opc_str);
				opc=strtol(opc_str.c_str(),&pEnd,10);
				if (*pEnd)
				{
					cout << endl << ace.texto("� necess�rio introduzir um valor inteiro!") << endl << endl;
					opc=0;
				}
				else if (opc<1 || opc>3)
				{
					cout << endl << ace.texto("Op��o inv�lida!") << endl << endl;
				}
			}
			
			//Abrir o ficheiro dos erros criado durante a leitura do ficheiro
			if (opc==1)
			{
				if (fich_OS)
				{
					f_ent.open("log_erros_comb_leit.txt");
				}
				else
				{
					f_ent.open("log_erros_comb_OS.txt");
				}
				while (getline(f_ent, aux_comb, '\n'))
				{
					cout << ace.texto(aux_comb) << endl;
				}
				f_ent.close();
				opc=0;
				//Depois de abrir o ficheiro, o utilizador tem de escolher se pretende continuar com o programa ou inserir um novo ficheiro de combina��es
				while (opc<1 || opc>2)
				{
					cout << "Pretende:" << endl << "1.Continuar com o programa;" << endl << "2.Introduzir outro ficheiro das combina" << ace.texto("��es") << endl << endl;
					getline(cin,opc_str);
					opc=strtol(opc_str.c_str(),&pEnd,10);
					if (*pEnd)
					{
						cout << endl << ace.texto("� necess�rio introduzir um valor inteiro!") << endl << endl;
						opc=0;
					}
					else if (opc<1 || opc>2)
					{
						cout << endl << ace.texto("Op��o inv�lida!") << endl << endl;
					}
				}
				opc++;
			}
			
			//Antes de introduzir um novo ficheiro, � necess�rio apagar todas as combina��es introduzidas com o ficheiro atual
			if (opc==3)
			{
				lista_comb.clear();
				return false;
			}
		}
		return true;
	}
	catch (fich_exis)
	{
		cout << "O ficheiro introduzido n" << ace.texto("�") << "o existe na pasta onde se encontra o programa!" << endl;
		return false;
	}
	catch (fich_vali_car)
	{
		f_out.close();
		f_ent.close();
		cout << "As caracter" << ace.texto("�") << "sticas do ficheiro " << ace.texto("n�o est�o ") << "bem definidas!" << endl;
		return false;
	}
	catch (fich_vali_nao_OS)
	{
		f_out.close();
		f_ent.close();
		cout << "O ficheiro introduzido tem caracter" << ace.texto("�") << "sticas das OS! Por favor introduze um ficheiro correspondente " << ace.texto("�") << "s leituras!" << endl;
		return false;
	}
	catch (fich_vali_OS)
	{
		f_out.close();
		f_ent.close();
		cout << "O ficheiro introduzido n" << ace.texto("�") << "o tem caracter" << ace.texto("�") << "sticas das OS! Por favor introduze um ficheiro correspondente " << ace.texto("�") << "s OS!" << endl;
		return false;
	}
	catch (fich_vali_OS_car)
	{
		f_out.close();
		f_ent.close();
		cout << "As caracter" << ace.texto("�") << "sticas existentes no ficheiro das OSs n" << ace.texto("�") << "o coincide com as do ficheiro das leituras!" << endl;
		return false;
	}
};

list<string> Combinacoes::get_car_DTC()
{
	return lista_comb.front().get_car_DTC();
}

list<string> Combinacoes::get_car_EB()
{
	return lista_comb.front().get_car_EB();
}

list<string> Combinacoes::get_car_DTC_EB()
{
	return lista_comb.front().get_car_DTC_EB();
}

void Combinacoes::apagar_dados()
{
	lista_comb.clear();
};

list<double> Combinacoes::get_prop_leituras(DTC dtc_aux, EB eb_aux, DTC_EB dtc_eb_aux, int num_EB)
{
	Acentos ace;
	string ds, pr_str;
	int n_min_eb, n_max_eb;
	double val_prop;
	list<string> dtc_val_aux, eb_val_aux, dtc_eb_val_aux;
	list<string> dias_semana = {"domingo", "segunda-feira", "ter�a-feira", "quarta-feira", "quinta-feira", "sexta-feira", "s�bado"};
	list<string>::iterator it_str;
	list<Combinacao>::iterator it_comb;
	list<double> dou_aux;
	bool encontrou;
	char * pEnd = NULL;
	Combinacao comb_aux;
	
	//Para cada dia da semana, encontrar a combina��o que cont�m a propor��o desejada
	for (it_str=dias_semana.begin(); it_str!=dias_semana.end(); it_str++)
	{
		encontrou=false;
		it_comb=lista_comb.begin();
	
		//O programa vai tentar encontrar a combina��o que coincide com os dados fornecidos e sai do ciclo se verificar as combina��es ou se encontrar a combina��o desejada
		while (it_comb!=lista_comb.end() && !encontrou)
		{
			//Valor das caracter�sticas do DTC da combina��o
			dtc_val_aux=(*it_comb).get_val_DTC();
			//Valor das caracter�sticas da EB da combina��o
			eb_val_aux=(*it_comb).get_val_EB();
			//Valor das caracter�sticas do DTC+EB da combina��o
			dtc_eb_val_aux=(*it_comb).get_val_DTC_EB();
			//O programa vai verificar se as caracter�sticas dos dados introduzidos coincidem com os da combina��o
			//Se os valores das caracter�sticas n�o forem exatamente iguais, � ignorada esta combina��o e passa para a seguinte
			if (dtc_aux.comparar_val(dtc_val_aux) && eb_aux.comparar_val(eb_val_aux) && dtc_eb_aux.comparar_val(dtc_eb_val_aux))
			{
				ds=(*it_comb).get_dia_semana();
				//Se o dia da semana n�o coincidir com o dia da semana da combina��o, � ignorada esta combina��o e passa para a seguinte
				if (*it_str==ds)
				{
					n_min_eb=(*it_comb).get_num_min_EBs();
					n_max_eb=(*it_comb).get_num_max_EBs();
					//Se se estiverem a considerar o n�mero de EBs que existem no DTC, o n�mero tem de estar compreendido entre o n�mero m�nimo de EBs e o n�mero m�ximo de EBs
					//Se isso n�o acontecer, � ignorada esta combina��o e passa para a seguinte
					//Se for v�lido, adiciona-se o valor da propor��o da combina��o a uma list<double> e o dia da semana utilizado a uma list<string> para que depois estes valores sejam adicionados a uma vari�vel Dia_sem_prop
					//O valor "encontrou" passa a ser true
					if (!qtd_EBs || (num_EB>=n_min_eb && num_EB<=n_max_eb))
					{
						val_prop=(*it_comb).get_prop();
						dou_aux.push_back(val_prop);
						encontrou=true;
					}
				}
			}
			it_comb++;
		}
		//Se n�o encontrou nenhuma combina��o, o programa ir� fazer o seguinte:
		//1�: Escrever a combina��o que n�o existe no grupo das combina��es
		//2�: Pedir ao utilizador que insire a propor��o a utilizar para esta combina��o
		//3�: Adicionar a combina��o com a propor��o inserida pelo utilizada ao conjunto das combina��es encontradas no ficheiro das combina��es
		if (!encontrou)
		{
			cout << ace.texto("A seguinte combina��o n�o existe no ficheiro introduzido:") << endl;
			cout << "DTC:" << endl;
			dtc_aux.escrever();
			cout << "EB:" << endl;
			eb_aux.escrever();
			cout << "DTC + EB:" << endl;
			dtc_eb_aux.escrever();
			cout << ace.texto("N�mero de EBs: ") << num_EB << endl;
			cout << "Dia da semana: " << *it_str << endl << endl;
			val_prop=-1;
			while (val_prop>1 || val_prop<0)
			{
				cout << ace.texto("Propor��o que pretende utilizar (de 0 a 1): ");
				getline(cin,pr_str);
				val_prop=strtod(pr_str.c_str(), &pEnd);
				if (*pEnd)
				{
					cout << endl << ace.texto("� necess�rio introduzir um double que esteja compreendido entre 0 e 1!") << endl << endl;
					val_prop=-1;
				}
				else if (val_prop>1 || val_prop<0)
				{
					cout << ace.texto("O valor introduzido n�o � v�lido!") << endl << endl;
				}
			}
			if (!qtd_EBs)
			{
				comb_aux.criar_combinacao(dtc_aux, eb_aux, dtc_eb_aux, *it_str, "", 0, 0, val_prop);
			}
			else
			{
				comb_aux.criar_combinacao(dtc_aux, eb_aux, dtc_eb_aux, *it_str, "", num_EB, num_EB, val_prop);
			}
			lista_comb.push_back(comb_aux);
			dou_aux.push_back(val_prop);
			comb_aux.apagar_dados();
		}
	}
	return dou_aux;
};

double Combinacoes::get_prop_os(DTC dtc_aux, EB eb_aux, DTC_EB dtc_eb_aux, int num_EB, string ds, string n_os)
{
	Acentos ace;
	string dia_sem, pr_str, os_nome;
	int n_min_eb, n_max_eb;
	double val_prop;
	list<string> dtc_val_aux, eb_val_aux, dtc_eb_val_aux;
	list<Combinacao>::iterator it_comb;
	bool encontrou=false;
	char * pEnd = NULL;
	Combinacao comb_aux;
	
	it_comb=lista_comb.begin();
	//O programa vai tentar encontrar a combina��o que coincide com os dados fornecidos e sai do ciclo se verificar as combina��es ou se encontrar a combina��o desejada
	while (it_comb!=lista_comb.end() && !encontrou)
	{
		//Valor das caracter�sticas do DTC da combina��o
		dtc_val_aux=(*it_comb).get_val_DTC();
		//Valor das caracter�sticas da EB da combina��o
		eb_val_aux=(*it_comb).get_val_EB();
		//Valor das caracter�sticas do DTC+EB da combina��o
		dtc_eb_val_aux=(*it_comb).get_val_DTC_EB();
		//O programa vai verificar se as caracter�sticas dos dados introduzidos coincidem com os da combina��o
		//Se os valores das caracter�sticas n�o forem exatamente iguais, � ignorada esta combina��o e passa para a seguinte
		if (dtc_aux.comparar_val(dtc_val_aux) && eb_aux.comparar_val(eb_val_aux) && dtc_eb_aux.comparar_val(dtc_eb_val_aux))
		{
			dia_sem=(*it_comb).get_dia_semana();
			//Se o dia da semana n�o coincidir com o dia da semana da combina��o, � ignorada esta combina��o e passa para a seguinte
			if (ds==dia_sem)
			{
				os_nome=(*it_comb).get_nome_OS();
				//Se o nome da OS n�o coincidir com o nome da OS da combina��o, � ignorada esta combina��o e passa para a seguinte
				if (os_nome==n_os)
				{
					n_min_eb=(*it_comb).get_num_min_EBs();
					n_max_eb=(*it_comb).get_num_max_EBs();
					//Se se estiverem a considerar o n�mero de EBs que existem no DTC, o n�mero tem de estar compreendido entre o n�mero m�nimo de EBs e o n�mero m�ximo de EBs
					//Se isso n�o acontecer, � ignorada esta combina��o e passa para a seguinte
					//Se for v�lido, adiciona-se o valor da propor��o da combina��o a uma list<double> e o dia da semana utilizado a uma list<string> para que depois estes valores sejam adicionados a uma vari�vel Dia_sem_prop
					//O valor "encontrou" passa a ser true
					if (!qtd_EBs || (num_EB>=n_min_eb && num_EB<=n_max_eb))
					{
						val_prop=(*it_comb).get_prop();
						encontrou=true;
					}
				}
			}
		}
		it_comb++;
	}
	//Se n�o encontrou nenhuma combina��o, o programa ir� fazer o seguinte:
	//1�: Escrever a combina��o que n�o existe no grupo das combina��es
	//2�: Pedir ao utilizador que insire a propor��o a utilizar para esta combina��o
	//3�: Adicionar a combina��o com a propor��o inserida pelo utilizada ao conjunto das combina��es encontradas no ficheiro das combina��es
	if (!encontrou)
	{
		cout << ace.texto("A seguinte combina��o n�o existe no ficheiro introduzido:") << endl;
		cout << "DTC:" << endl;
		dtc_aux.escrever();
		cout << "EB:" << endl;
		eb_aux.escrever();
		cout << "DTC + EB:" << endl;
		dtc_eb_aux.escrever();
		cout << ace.texto("N�mero de EBs: ") << num_EB << endl;
		cout << "Dia da semana: " << ds << endl;
		cout << "Nome da OS: " << n_os << endl << endl;
		val_prop=-1;
		while (val_prop>1 || val_prop<0)
		{
			cout << ace.texto("Propor��o que pretende utilizar (de 0 a 1): ");
			getline(cin,pr_str);
			val_prop=strtod(pr_str.c_str(), &pEnd);
			if (*pEnd)
			{
				cout << endl << ace.texto("� necess�rio introduzir um double que esteja compreendido entre 0 e 1!") << endl << endl;
				val_prop=-1;
			}
			else if (val_prop>1 || val_prop<0)
			{
				cout << ace.texto("O valor introduzido n�o � v�lido!") << endl << endl;
			}
		}
		if (!qtd_EBs)
		{
			comb_aux.criar_combinacao(dtc_aux, eb_aux, dtc_eb_aux, ds, n_os, 0, 0, val_prop);
		}
		else
		{
			comb_aux.criar_combinacao(dtc_aux, eb_aux, dtc_eb_aux, ds, n_os, num_EB, num_EB, val_prop);
		}
		lista_comb.push_back(comb_aux);
		comb_aux.apagar_dados();
	}
	return val_prop;
};

