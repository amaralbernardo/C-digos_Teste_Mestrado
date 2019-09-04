#ifndef COMBINACOES
#define COMBINACOES

class Combinacoes{
private:
	
	//Variável que contém todas as combinações retiradas do ficheiro, seja das OSs, seja das leituras
	//Nota as combinações das OSs e das leituras nunca estão presentes na mesma variável
	list<Combinacao> lista_comb;
	//Variável que indica se se está a considerar a divisão dos DTCs por EBs ou não
	bool qtd_EBs;
	//Estrutura que indica que o ficheiro com o nome introduzido não existe
	struct fich_exis{};
	//Estrutura que indica que as caraterísticas do ficheiro não estão bem definidas
	struct fich_vali_car{};
	//Estrutura que indica que se utilizou um ficheiro relativo às OSs em vez das leituras
	struct fich_vali_nao_OS{};
	//Estrutura que indica que se utilizou um ficheiro que não está associado às OSs
	struct fich_vali_OS{};
	//Estrutura que indica que as caraterísticas introduzidas no ficheiro das OSs não coincidem com o ficheiro introduzido com o ficheiro das leituras
	struct fich_vali_OS_car{};
	
	//Função que verifica as características existentes no ficheiro.
	//Se as características forem válidas, devolve uma variável da classe "Combinação"
	//Caso contrário, uma das estruturas enunciadas acima é ativada e o ficheiro não é válido
	Combinacao verificar_car(string, bool, list<string>, list<string>, list<string>);
	//Função que verifica os valores de uma combinação existente no ficheiro introduzido
	//Se a combinação for válida, o programa devolve uma string vazia
	//Caso contrário, devolve o tipo de erro encontrado, que será posteriormente gravado num ficheiro log de erros
	string verificar_val(string, Combinacao, int, bool);
	//Função que verifica se a string introduzida é um dia de semana válido e devolve o valor de TRUE se for válido e FALSE caso contrário
	bool verificar_dia_semana(string);
	//Função que devolve a proporção a ser utilizada para uma determinada combinação
	double gerar_val_prop(double, double, double);
	//Função que gera e devolve um valor entre 0 e 1
	double gerar_valor();
public:
	//construtor
	Combinacoes(){};
	//Destrutor
	~Combinacoes(){};
	//Função que lê e analisa o ficheiro introduzido. Se for válido, o ficheiro retira as informações do ficheiro e grava-as na variável "lista_comb"
	//No caso do ficheiro ser válido, devolve o valor TRUE. Caso contrário, devolve o valor FALSE
	bool abrir_ficheiro(string, bool, list<string>, list<string>, list<string>);
	//Função que devolve as caraterísticas associadas aos DTCs
	list<string> get_car_DTC();
	//Função que devolve as características associadas às EBs
	list<string> get_car_EB();
	//Função que devolve as características associadas aos DTCs e EBs
	list<string> get_car_DTC_EB();
	//Função que apaga os dados da variável "lista_comb"
	void apagar_dados();
	//Função que gera e devolve uma lista de proporções para os dados introduzidos para serem simulados, para as leituras
	list<double> get_prop_leituras(DTC, EB, DTC_EB, int);
	//Função que gera e devolve uma proporção para a OS a ser realizada no simulador
	double get_prop_os(DTC, EB, DTC_EB, int, string, string);
};

#endif
