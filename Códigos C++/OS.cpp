#ifndef os
#define os

#include <iostream>
#include <string>

using namespace std;

class OS{
private:
	string nome_OS;
	string id_DTC;
	string id_EB;
	string dia_semana;
	double h_realizar;
	double prop;
public:
	//construtor
	OS(){};
	//Destrutor
	~OS(){};
	
	void set_nome_OS(string n_OS)
	{
		nome_OS=n_OS;
	}
	
	void set_id_DTC(string id_DTC_aux)
	{
		id_DTC=id_DTC_aux;
	}
	
	void set_id_EB(string id_EB_aux)
	{
		id_EB=id_EB_aux;
	}
	
	void set_dia_semana(string ds)
	{
		dia_semana=ds;
	}
	
	void set_h_realizar(double hr)
	{
		h_realizar=hr;
	}
	
	void set_prop(double prop_aux)
	{
		prop=prop_aux;
	}
	
	string get_nome_OS()
	{
		return nome_OS;
	}
	
	string get_id_DTC()
	{
		return id_DTC;
	}
	
	string get_id_EB()
	{
		return id_EB;
	}
	
	string get_dia_semana()
	{
		return dia_semana;
	}
	
	double get_prop()
	{
		return prop;
	}
	
	double get_h_realizar()
	{
		return h_realizar;
	}
};

#endif
