%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <map>
#include <string>
extern int yylex(void);
extern int yylineno;
extern char* yytext;
void yyerror(char const*);	

struct Node
{
	int loc;
	int type;
	int value;
};
using namespace std;

int get_mem();
void free_mem(int);
int type_compatible(int,int);
Node arithmetic(int,int,string,int,int);
void inc_dcr(char*,int,int,char);
void assignment(string,int,int,int,int,int);
Node f(char*,int,int,int);
void free_all(int);

int location[1000];
int size[3]={4,8,1};
map <string,int> type_map;
map <string,int> size_map;

%}

%union {
	struct Node
	{
		int loc;
		int type;
	}node;
	int type_val;
	char str[500];

}

%token PLUS MINUS STAR DIV MOD INC DCR  UNEQUAL NOT LT GT EQUAL LTE GTE ASSIGN BITWISE_OR BITWISE_AND OR AND LEFTSHIFT RIGHTSHIFT BITWISE_NOT LP RP SLP SRP FLP FRP COMMA SEMI DOT COLON BACKSLASH SQ DQ ELSE ERROR STRING_CONSTANT
%token <str> NUM FNUM ID CHAR_CONSTANT
%token <type_val> DATATYPE FOR WHILE IF IFE
%type <node> s stm_list stm simple_stm rexpr srexpr expr term factor factor1 temp_id
%start s

%%
s : stm_list

stm_list : stm stm_list {}
	| %empty {}
	
stm : simple_stm SEMI
	| FLP stm_list FRP {}
	| WHILE {printf("l%d:\nadd $t0,$t0,$zero\n",$1);} LP rexpr {printf("beq $t%d,$zero,e%d\n",$4.loc,$1);free_mem($4.loc);} RP stm {printf("j l%d\ne%d:\nadd $t0,$t0,$zero\n",$1,$1);} 
	| FOR LP simple_stm {printf("l%d:\nadd $t0,$t0,$zero\n",$1); } SEMI rexpr  {printf("beq $t%d,$zero,e%d\n",$6.loc,$1);free_mem($6.loc);} SEMI {printf("j f%d\nef%d:\nadd $t0,$t0,$zero\n",$1,$1);} simple_stm {printf("j l%d\n",$1);} RP {printf("f%d:\nadd $t0,$t0,$zero\n",$1);} stm {printf("j ef%d\ne%d:\nadd $t0,$t0,$zero\n",$1,$1);}
	| IF LP rexpr RP {printf("beq $t%d,$zero,end_l%d\n",$3.loc,$1);free_mem($3.loc);} stm {printf("j end_l%d\n",$1);printf("end_l%d:\nadd $t0,$t0,$zero\n",$1);}
	| IFE LP rexpr {printf("beq $t%d,$zero,else_l%d\n",$3.loc,$1);free_mem($3.loc);} RP stm {printf("j end_l%d\n",$1);printf("else_l%d:\nadd $t0,$t0,$zero\n",$1);} ELSE stm {printf("j end_l%d\n",$1);printf("end_l%d:\nadd $t0,$t0,$zero\n",$1);}
	

	
simple_stm : ID ASSIGN rexpr   {assignment(string($1),$3.type,$3.loc,0,0,0);free_all(0);}
	| ID SLP rexpr SRP ASSIGN rexpr {assignment(string($1),$6.type,$6.loc,$3.type,$3.loc,1);free_all(0);}
	| DATATYPE ID   {if(type_map.find(string($2))==type_map.end())type_map[string($2)]=$1;else yyerror("Variable redeclared");free_all(0);}
	| DATATYPE ID ASSIGN rexpr  {if(type_map.find(string($2))==type_map.end()){type_map[string($2)]=$1;assignment(string($2),$4.type,$4.loc,0,0,0);}else {yyerror("Variable redeclared");};free_all(0);}
	| DATATYPE ID SLP NUM SRP  {if(type_map.find(string($2))==type_map.end()){size_map[string($2)]=atoi($4);type_map[string($2)]=$1;}else {yyerror("Variable redeclared");};free_all(0);}
	| factor1 {free_all(0);}
	| %empty {}

rexpr  : rexpr OR srexpr {Node n = arithmetic($1.loc,$3.loc,"||",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;} 
	| rexpr AND srexpr {Node n = arithmetic($1.loc,$3.loc,"&&",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| srexpr {$$=$1;} 

srexpr : srexpr EQUAL expr {Node n = arithmetic($1.loc,$3.loc,"==",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| srexpr UNEQUAL expr {Node n = arithmetic($1.loc,$3.loc,"!=",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| srexpr LT expr {Node n = arithmetic($1.loc,$3.loc,"<",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| srexpr GT expr {Node n = arithmetic($1.loc,$3.loc,">",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| srexpr LTE expr {Node n = arithmetic($1.loc,$3.loc,"<=",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| srexpr GTE expr  {Node n = arithmetic($1.loc,$3.loc,">=",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| expr {$$ = $1;}

expr : expr PLUS term {Node n = arithmetic($1.loc,$3.loc,"+",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| expr MINUS term {Node n = arithmetic($1.loc,$3.loc,"-",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| term {$$ = $1;}

term : term STAR factor { Node n = arithmetic($1.loc,$3.loc,"*",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| term DIV factor { Node n = arithmetic($1.loc,$3.loc,"/",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| term MOD factor { Node n = arithmetic($1.loc,$3.loc,"%",$1.type,$3.type);$$.loc = n.loc;$$.type = n.type;}
	| factor {$$ = $1;}

factor : NUM {printf("li $t%d,%d\n",$$.loc=get_mem(),atoi($1));$$.type=0;}
	| FNUM {printf("li.s $f%d,%f\n",$$.loc=get_mem(),atof($1));$$.type=1;}
	| LP rexpr RP {$$=$2;}
	| temp_id {$$=$1;}
	| MINUS NUM {printf("li $t%d,-%d\n",$$.loc=get_mem(),atoi($2));$$.type=0;}
	| MINUS FNUM {printf("li.s $t%d,-%f\n",$$.loc=get_mem(),atof($2));$$.type=1;}
	| MINUS temp_id {printf("neg $t%d,$t%d\n",$2.loc,$2.loc);$$=$2;}
	| factor1 {$$=$1;}
	| DCR NUM {printf("li $t%d,%d\n",$$.loc=get_mem(),atoi($2)-1);$$.type=0;}
	| INC NUM {printf("li $t%d,%d\n",$$.loc=get_mem(),atoi($2)+1);$$.type=0;}
	| NUM DCR {printf("li $t%d,%d\n",$$.loc=get_mem(),atoi($1));$$.type=0;}
	| NUM INC {printf("li $t%d,%d\n",$$.loc=get_mem(),atoi($1));$$.type=0;}
	| NOT factor {printf("seq $t%d,$t%d,$zero\n",$2.loc,$2.loc);$$ = $2;}
	| CHAR_CONSTANT {printf("li $t%d,%d\n",$$.loc=get_mem(),int($1[0]));$$.type=3;}

factor1 : DCR ID {inc_dcr($2,1,$$.loc=get_mem(),'-');$$.type=type_map[string($2)];}
	| INC ID {inc_dcr($2,1,$$.loc=get_mem(),'+');$$.type=type_map[string($2)];}
	| ID DCR {inc_dcr($1,2,$$.loc=get_mem(),'-');$$.type=type_map[string($1)];}
	| ID INC {inc_dcr($1,2,$$.loc=get_mem(),'+');$$.type=type_map[string($1)];}

temp_id : ID {Node n = f($1,0,0,0);$$.loc=n.loc;$$.type=n.type;}
	| ID SLP rexpr SRP {Node n = f($1,$3.type,$3.loc,1);$$.loc=n.loc;$$.type=n.type;}
%%

Node f(char* a,int offset_type,int offset_location,int flag)
{
	Node n;
	if(type_map.find(string(a))==type_map.end())
	{
		yyerror("Unknown variable");
		return n;
	}
	n.type=type_map[string(a)];
	if(flag==0)
	{
		printf("lw $t%d,%sval\n",n.loc=get_mem(),a);
		
	}
	else
	{
		if(offset_type != 0)
		{
			yyerror("Index must be integer");
			return n;
		}
		int temp,temp2;
		printf("la $t%d,%sval\n",temp=get_mem(),a);
		printf("li $t%d,%d\n",temp2=get_mem(),size[n.type]);
		printf("mult $t%d,$t%d\n",offset_location,temp2);
		printf("mflo $t%d\n",offset_location);
		printf("add $t%d,$t%d,$t%d\n",temp,temp,offset_location);
		printf("lw $t%d,($t%d)\n",n.loc=get_mem(),temp);
		free_mem(offset_location);
		free_mem(temp);
		free_mem(temp2);
	}
	return n;
}

void inc_dcr(char *a,int way,int b,char symbol)
{
	/* 
	1: DEC ID, INC ID
	2: ID DEC, ID INC
	*/
	if(way == 2)
	{
		printf("lw $t%d,%sval\n",b,a);
	}
	int temp1 = get_mem();
	printf("lw $t%d,%sval\n",temp1,a);
	if(symbol == '+')
		printf("addi $t%d,$t%d,1\n",temp1,temp1);
	else
		printf("addi $t%d,$t%d,-1\n",temp1,temp1);
	printf("sw $t%d,%sval\n",temp1,a);
	if (way == 1)
	{
		printf("lw $t%d,%sval\n",b,a);
	}
	free_mem(temp1);
}

void assignment(string a,int b,int c,int index_type,int offset_location,int flag)
{
	if(type_map.find(a)==type_map.end())
	{
		yyerror("Variable Undeclared");
		return;
	}
	if(flag==1 and index_type!=0)
	{
		yyerror("index must be integer");
		return;
	}
	int temp_type = type_map[a];
	char bla[a.length()+1]; 
	strcpy(bla,a.c_str());
	if(temp_type==b && b!=1)
	{
		if(flag==0)
		{
			printf("sw $t%d,%sval\n",c,bla);
		}
		else
		{
			int temp = get_mem();
			int temp2 = get_mem();
			printf("la $t%d,%sval\n",temp,bla);
			printf("li $t%d,%d\n",temp2=get_mem(),size[temp_type]);
			printf("mult $t%d,$t%d\n",offset_location,temp2);
			printf("mflo $t%d\n",offset_location);
			printf("add $t%d,$t%d,$t%d\n",temp,temp,offset_location);
			printf("sw $t%d,($t%d)\n",c,temp);
			free_mem(temp);
			free_mem(temp2);
			free_mem(offset_location);
		}
		free_mem(c);
	}
	else if (temp_type == 1 && b==1)
	{
		if(flag==0)
		{
			printf("swc1 $f%d,%sval\n",c,bla);

		}
		else
		{
			int temp = get_mem();
			int temp2 = get_mem();
			printf("la $t%d,%sval\n",temp,bla);
			printf("li $t%d,%d\n",temp2=get_mem(),size[temp_type]);
			printf("mult $t%d,$t%d\n",offset_location,temp2);
			printf("mflo $t%d\n",offset_location);
			printf("add $t%d,$t%d,$t%d\n",temp,temp,offset_location);
			printf("sw $t%d,($t%d)\n",c,temp);
			free_mem(temp);
			free_mem(temp2);
			free_mem(offset_location);
		}
		free_mem(c);
	}
	else if (temp_type==0 && b==1)
	{
		if(flag==0)
		{
			printf("swc1 $f%d,%sval\n",c,bla);
		}
	}
	else
		yyerror("Type Mismatch");

}

int get_mem()
{
	for(int i=0;i<1000;i++)
	{
		if(location[i]==0)
		{
			location[i]=1;
			return i;
		}
	}		
}

void free_mem(int a)
{
	location[a]=0;
	return;
}

void free_all(int a)
{
	for(int i=0;i<1000;i++)
		location[i]=0;
}

int type_compatible(int a,int b)
{
	if(a==b)
		return 1;
	else
		return 0;
}

Node arithmetic(int a,int b,string c,int t1,int t2)
{
	Node n;
	n.loc = a;
	if(t1==t2 && t1!=1 )
	{ 
		switch(c[0])
		{
			case '+':
				printf("add $t%d,$t%d,$t%d\n",a,a,b);
				break;
			case '-':
				printf("sub $t%d,$t%d,$t%d\n",a,a,b);
				break;
			case '*':
				printf("mult $t%d,$t%d\nmflo $t%d\n",a,b,a);
				break;
			case '/':
				printf("div $t%d,$t%d\nmflo $t%d\n",a,b,a);
				break;
			case '%':
				printf("div $t%d,$t%d\nmfhi $t%d\n",a,b,a);
				break;
			case '=':
				printf("seq $t%d,$t%d,$t%d\n",a,a,b);
				break;
			case '<':
				if(c.length()>1)
					printf("sle $t%d,$t%d,$t%d\n",a,a,b);
				else
					printf("slt $t%d,$t%d,$t%d\n",a,a,b);
				break;
			case '>':
				if(c.length()>1)
					printf("sge $t%d,$t%d,$t%d\n",a,a,b);
				else
					printf("sgt $t%d,$t%d,$t%d\n",a,a,b);
				break;
			case '|':
				printf("or $t%d,$t%d,$t%d\n",a,a,b);
				break;
			case '&':
				printf("and $t%d,$t%d,$t%d\n",a,a,b);
				break;
			case '!':
				printf("seq $t%d,$t%d,$t%d\n",a,a,b);
				printf("seq $t%d,$t%d,$zero\n",a,a);
				break;
		}
		n.type = 0;
	}
	else if (t1 + t2 <= 1 || (t1==1 && t2==1) )
	{
		if(t1 == 0)
		{
			printf("mtc1 $t%d,$f%d\n",a,a);
			printf("cvt.s.w $f%d,$f%d\n",a,a);
		}
		if(t2 == 0)
		{
			printf("mtc1 $t%d,$f%d\n",b,b);
			printf("cvt.s.w $f%d,$f%d\n",b,b);
		}
		switch(c[0])
		{
			case '+':
				printf("add.s $f%d,$f%d,$f%d\n",a,a,b);
				n.type=1;
				
				break;
			case '-':
				printf("sub.s $f%d,$f%d,$f%d\n",a,a,b);
				n.type=1;
				break;
			case '*':
				printf("mul.s $f%d,$f%d,$f%d\n",a,a,b);
				n.type=1;
				break;
			case '/':
				printf("div.s $f%d,$f%d,$f%d\n",a,a,b);
				n.type=1;
				break;
			case '%':
				yyerror("Remainder not defined for float");
				break;
			case '=':
				printf("c.eq.s $f%d,$f%d\n",a,b);
				printf("cfc1 $t%d,$25\n",a);
				printf("andi $t%d 1\n",a);
				n.type=0;
				break;
			case '<':
				printf("c.lt.s $f%d,$f%d\n",a,b);
				printf("cfc1 $t%d,$25\n",a);
				printf("andi $t%d 1\n",a);
				if(c.length()>1)
				{
					int a1;
					printf("c.eq.s $f%d,$f%d\n",a,b);
					printf("cfc1 $t%d,$25\n",a1=get_mem());
					printf("andi $t%d 1\n",a1);
					printf("or $t%d,$t%d,$t%d\n",a,a,a1);
					free_mem(a1);		
				}
				n.type=0;
				break;
			case '>':
				printf("c.gt.s $f%d,$f%d\n",a,b);
				printf("cfc1 $t%d,$25\n",a);
				printf("andi $t%d 1\n",a);
				if(c.length()>1)
				{
					int a1;
					printf("c.eq.s $f%d,$f%d\n",a,b);
					printf("cfc1 $t%d,$25\n",a1=get_mem());
					printf("andi $t%d 1\n",a1);
					printf("or $t%d,$t%d,$t%d\n",a,a,a1);
					free_mem(a1);		
				}
				n.type=0;
				break;
			case '|':
				printf("or $t%d,$t%d,$t%d\n",a,a,b);
				n.type=0;
				break;
			case '&':
				printf("and $t%d,$t%d,$t%d\n",a,a,b);
				n.type=0;
				break;
			case '!':
				printf("c.eq.s $f%d,$f%d\n",a,b);
				printf("cfc1 $t%d,$25\n",a);
				printf("andi $t%d 1\n",a);
				printf("seq $t%d,$t%d,$zero\n",a,a);
				n.type=0;
				break;
		}
	}
	else
	{
		yyerror("Type Incompatible");
	}
	free_mem(b);
	return n;
}

void yyerror(char const* msg)
{
	fprintf(stderr,"Error : %s at line_number%d\n",msg,yylineno);
}

int main()
{
	/*int a;
	while(a=yylex())
	{
		printf("%s %d\n",yytext,a);
	}*/
	printf(".text\n.globl main\nmain:\n");
	for(int i=0;i<1000;i++)
		location[i]=0;
	int result = yyparse();
	if(result == 0)
	{ 	
		fprintf(stderr,"The program is syntactically valid\n");
	}
	else
		fprintf(stderr,"syntax errors detected\n");
	
	printf("li $v0,10\nsyscall\n");
	printf("\n.data\n");
	for(map<string,int>::iterator it = type_map.begin(); it != type_map.end(); ++it) 
	{
		string a = it->first;
		string b = a;
		char bla[a.length()+1]; 
		if (size_map.find(a)==size_map.end())
		{
			strcpy(bla,a.c_str());
			if(type_map[b]==0)
				printf("%sval: .word 0\n",bla);
			if(type_map[b]==1)
				printf("%sval: .float 0.0\n",bla);
			if(type_map[b]==3)
				printf("%sval: .byte 0\n",bla);
		}	
		else
		{
			strcpy(bla,a.c_str());
			if(type_map[b]==0)
				printf("%sval: .word 0:%d\n",bla,size_map[b]);			
			if(type_map[b]==1)
				printf("%sval: .float 0.0:%d\n",bla,size_map[b]);
			if(type_map[b]==3)
				printf("%sval: .byte 0:%d\n",bla,size_map[b]);
		}
	}
	return result;
}

