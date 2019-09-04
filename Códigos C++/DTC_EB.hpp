#ifndef dtc_eb
#define dtc_eb

class DTC_EB{
private:
	list<Caracteristica> list_carac;
public:
	//construtor
	DTC_EB(){};
	//Destrutor
	~DTC_EB(){};
	void add_carac(string);
	list<string> get_carac();
	int get_num_car();
	void add_valor(string, int);
	bool comparar_tamanhos();
	bool comparar_car(list<string>);
	list<string> get_valor();
	bool comparar_val(list<string>);
	void escrever();
	void apagar_dados();
};

#endif
