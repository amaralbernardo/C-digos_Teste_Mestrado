#ifndef ESTRUTURA
#define ESTRUTURA

class Estrutura{
private:
	list<Rede> lis_rede;
	list<string> id_rede;
	list<OS> lis_os;
	list<list<string>> esc_fich;
	struct fich_exis{};
	struct fich_vali_ID_meio{};
	struct fich_vali_car_vazio{};
	struct fich_vali_falta_ID_EB{};
	struct fich_vali_falta_ID{};
	struct fich_vali_car_dif{};
	struct fich_vali_car_mais{};
	struct fich_sem_DTCs{};
	
	Rede verificar_car_DTC(string, list<string>);
	bool verificar_ID_DTC(string);
	string verificar_val_DTC(string, Rede, int);
	int verificar_associar_ID_DTC(string);
	bool verificar_ID_EB(string, int);
	string verificar_val_EB(string, EB, DTC_EB, int, int, int);
	bool verificar_Estrutura();
	string verificar_val_OS(string, int);
	struct tm dia_seguinte(struct tm);
	list<list<string>> escrever_ficheiro(struct tm, Rede, string, list<list<string>>);
	int numero_dias(int, int);
	double gerar_valor();
	string sucesso_insucesso(double, int, double);
public:
	//construtor
	Estrutura(){};
	//Destrutor
	~Estrutura(){};
	
	int abrir_ficheiro_DTC(string, list<string>);
	int abrir_ficheiro_EB(string, list<string>, list<string>);
	void add_prop(Combinacoes, bool);
	int abrir_ficheiro_OS(string);
	void simular_leituras(string);
	void simular_OS(string);
};

#endif
