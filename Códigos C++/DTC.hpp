#ifndef dtc
#define dtc

class DTC{
private:
	list<Caracteristica> list_carac;
public:
	//construtor
	DTC(){}; 
	//Destrutor
	~DTC(){};
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
