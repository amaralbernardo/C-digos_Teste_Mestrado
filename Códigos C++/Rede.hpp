#ifndef REDE
#define REDE

class Rede{
private:
	DTC dtc_rede;
	list<EB> lista_eb;
	list<DTC_EB> lista_dtc_eb;
	list<string> id_eb;
	list<list<double>> lista_prop;
	
public:
	//construtor
	Rede(){};
	//Destrutor 
	~Rede(){};
	
	void add_car_DTC(string);
	bool comparar_car_DTC(list<string>);
	int get_num_car_DTC();
	void add_val_DTC(string, int);
	bool verificar_ID_EB(string);
	void add_lista_eb(EB);
	void add_lista_dtc_eb(DTC_EB);
	void add_id_EB(string);
	bool verificar_rede();
	void apagar_EBs();
	void apagar_DTC_EBs();
	void apagar_ID_EBs();
	void add_prop(Combinacoes);
	DTC get_dtc_rede();
	list<string> get_id_eb();
	EB get_EB(int);
	DTC_EB get_DTC_EB(int);
	int get_num_EBs();
	list<EB> get_lista_eb();
	list<DTC_EB> get_lista_dtc_eb();
	list<list<double>> get_lista_prop();
	list<string> get_car_DTC();
	list<string> get_car_EB();
	list<string> get_car_DTC_EB();
};

#endif
