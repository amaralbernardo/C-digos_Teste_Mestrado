#ifndef COMBINACAO
#define COMBINACAO

class Combinacao{
private:
	//Variável correspondente ao DTC na combinação
	DTC dtc_comb;
	//Variável correspondente à EB na combinação
	EB eb_comb;
	//Variável correspondente ao DTC e à EB na combinação
	DTC_EB dtc_eb_comb;
	//Variável correspondente ao dia da semana da combinação
	string dia_semana;
	//Variável correspondente ao nome da OS da combinação (Nota: É igual a "" se o ficheiro a estudar for o das leituras)
	string nome_OS;
	//Variável correspondente ao número mínimo de EBs na combinação (Nota: Igual a 0 se "qtd_EBs"=FALSE na classe "Combinações", ou seja, é desconsiderado)
	int num_min_EBs;
	//Variável correspondente ao número máximo de EBs na combinação (Nota: Igual a 0 se "qtd_EBs"=FALSE na classe "Combinações", ou seja, é desconsiderado)
	int num_max_EBs;
	//Variável correspondente à proporção associada à combinação
	double prop;
public:
	//construtor
	Combinacao(){};
	//Destrutor
	~Combinacao(){};
	
	//Função que adiciona caraterísticas ao DTC
	void add_car_DTC(string);
	//Função que adiciona caraterísticas à EB
	void add_car_EB(string);
	//Função que adiciona caraterísticas ao conjunto DTC + EB
	void add_car_DTC_EB(string);
	//Função que devolve as características do DTC
	list<string> get_car_DTC();
	//Função que devolve as características da EB
	list<string> get_car_EB();
	//Função que devolve as características do conjunto DTC+EB
	list<string> get_car_DTC_EB();
	int get_num_car_DTC();
	int get_num_car_EB();
	int get_num_car_DTC_EB();
	//Função que adiciona valores à característica do DTC
	void add_val_DTC(string, int);
	//Função que adiciona valores à característica da EB
	void add_val_EB(string, int);
	//Função que adiciona valores à característica ao conjunto DTC+EB
	void add_val_DTC_EB(string, int);
	//Função que adiciona o dia da semana à combinação
	void set_dia_semana(string);
	//Função que adiciona o nome da OS à combinação
	void set_nome_OS(string);
	//Função que adiciona os valores do número mínimo e máximo das EBs a considerar na combinação
	void set_qtd_EBs(int, int);
	//Função que adiciona a proporção associada à combinação
	void set_prop(double);
	//Função que compara os valores e as caraterísticas associadas ao DTC. Se for diferentes, a função devolve FALSE, caso contrário, TRUE.
	bool comparar_tamanhos_DTC();
	//Função que compara os valores e as caraterísticas associadas à EB. Se for diferentes, a função devolve FALSE, caso contrário, TRUE.
	bool comparar_tamanhos_EB();
	//Função que compara os valores e as caraterísticas associadas ao conjunto DTC+EB. Se for diferentes, a função devolve FALSE, caso contrário, TRUE.
	bool comparar_tamanhos_DTC_EB();
	//Função que devolve os valores associados ao DTC
	list<string> get_val_DTC();
	//Função que devolve os valores associados à EB
	list<string> get_val_EB();
	//Função que devolve os valores associados ao conjunto DTC+EB
	list<string> get_val_DTC_EB();
	//Função que devolve os dias da semana
	string get_dia_semana();
	int get_num_min_EBs();
	int get_num_max_EBs();
	double get_prop();
	void criar_combinacao(DTC, EB, DTC_EB, string, string, int, int, double);
	void apagar_dados();
	string get_nome_OS();
};

#endif
