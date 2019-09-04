#ifndef ACENTOS
#define ACENTOS

#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <cctype>

using namespace std;

class Acentos{
private:
public:
	//construtor
	Acentos(){};
	//Destrutor
	~Acentos(){};
	
	string car_esp(string auxString)
	{
		string car_espe_char="àáâãçèéêìíòóôõùúÀÁÂÃÈÉÊÌÍÒÓÔÕÙÚ", aux;
		int pos_int;
		vector<int>::iterator subs;
		string::iterator pos;
		for (size_t i=0; i< auxString.size(); i++)
		{
			pos=find(car_espe_char.begin(), car_espe_char.end(), auxString.at(i));
			if (int(auxString.at(i))<0 && pos==car_espe_char.end())
			{
				auxString.erase(auxString.begin()+i);
				i--;
			}
		}
		return auxString;
	};
	
	string texto(string auxString)
	{
		string car_espe_char="àáâãçèéêìíòóôõùúÀÁÂÃÈÉÊÌÍÒÓÔÕÙÚ", aux;
		vector<int> car_espe_int = {-123, -96, -125, -58, -121, -118, -126, -120, -115, -95, -107, -94, -109, -28, -105, -93, -73, -75, -74, -57, -44, -112, -46, -34, -42, -29, -32, -30, -27, -21, -23};
		int pos_int;
		vector<int>::iterator subs;
		string::iterator pos;
		for (size_t i=0; i< auxString.size(); i++)
		{
			pos=find(car_espe_char.begin(), car_espe_char.end(), auxString.at(i));
			if (pos!=car_espe_char.end())
			{
				pos_int=pos-car_espe_char.begin();
				subs=car_espe_int.begin()+pos_int;
				aux=char(*subs);
				auxString.replace(i,1,aux);
			}
		}
		return auxString;
	};
	
	string espacos_branco(string auxString)
	{
		auxString.erase(remove_if(auxString.begin(), auxString.end(), ::isspace), auxString.end());
		return auxString;
	};
};

#endif
