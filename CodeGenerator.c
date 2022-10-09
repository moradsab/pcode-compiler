
#include    "CodeGenerator.h"

enum ldc{ADDRESS,VALUE};
int label=0,switch_flag=0,break_label,case_counter,ldc_flag=0,cast=0,const_switch_flag=0,switch_expr;

typedef struct symbol_table {
    struct variable *var;
    struct symbol_table* next;

} Symbol_table;

typedef struct variable {
    char *name;
    char *type;
    int address;
    int size;
} Variable;

Symbol_table *ST=NULL;

Symbol_table* find(char *var_name){
    Symbol_table* tmp=ST;
    while(tmp!=NULL&&tmp->next!=NULL && strcmp(tmp->var->name,var_name)!=0){
        tmp=tmp->next;
    }
    if(tmp!=NULL&&strcmp(tmp->var->name,var_name)==0){
        return tmp;
    }else{return NULL;}
}

void variable_type(Variable *variable,int type){
    switch(type){
        case INT:
			variable->type="int";
            break;
        case FLOAT:
            variable->type="float";
            break;
        case DOUBLE:
            variable->type="double";
            break;
        case STRUCT:
            variable->type="struct";
    }
}

void declaration(treenode *root,Symbol_table *st)
{
    if(!root){return;}
    leafnode* leaf=(leafnode*)root;
    if(root->hdr.type==TN_TYPE_LIST){
        if(((leafnode*)root->lnode)->hdr.type==TN_TYPE){
           variable_type(st->var,((leafnode*)root->lnode)->hdr.tok);
            st->var->size=1;
        }
        if(ST==NULL){st->var->address=5;}
        else{st->var->address=ST->var->address+ST->var->size;}
    }
    if(leaf->hdr.type==TN_IDENT)
    {
        st->var->name = leaf->data.sval->str;
    }
    declaration(root->lnode,st);
    declaration(root->rnode,st);
}

void add (treenode* root)
{
    Symbol_table *new_st;
    new_st=(Symbol_table*)malloc(sizeof(Symbol_table));
    if(new_st==NULL){return;}else{
        new_st->var=(Variable*)malloc(sizeof(Variable));
        if(new_st->var==NULL){return;}
    }
    declaration(root,new_st);
    new_st->next=ST;
    ST=new_st;
}


void ldc(char *id){
	if(find(id)!=NULL){
		printf("ldc %d\n",find(id)->var->address);
    }
    if(ldc_flag==VALUE){printf("ind\n");}
}

void create_st(treenode *root){
	if(!root){
		return;
	}

    if(root->hdr.which==FOR_T){
		for_node *forn=(for_node *)root;
		if(forn->hdr.type==TN_FUNC_DEF){ 
            create_st(forn->stemnt);
		}
	}
    if(root->hdr.type==TN_DECL){
		add(root);
		return;
	}
    
	create_st(root->lnode);
	create_st(root->rnode);
	
}

void free_ST(){
	Symbol_table *tmp_next;
	while(ST!=NULL){
		tmp_next=ST->next;
		free(ST->var);
        free(ST);
		ST=tmp_next;
	}
}

void print_variable(Variable *variable){
	printf("%2s %10s %10d %10d\n",variable->name,variable->type,variable->address,variable->size);
}

void print_st(Symbol_table* st){
	if(!st){return;}
	print_st(st->next);
	print_variable(st->var);
}

double calculate_value(treenode* root){

    if(root->hdr.which==LEAF_T){
        leafnode* leaf = (leafnode *) root;
        if(leaf->hdr.type==TN_INT){
            return leaf->data.ival;
        }else if(leaf->hdr.type==TN_REAL){
            if(leaf->hdr.tok==DOUBLE){
                return leaf->data.dval;
            }else if(leaf->hdr.tok==FLOAT){
                return leaf->data.fval;
            }
        }else if(leaf->hdr.type==TN_IDENT){
            if(strcmp(leaf->data.sval->str, "true")==0){
				return 1;
		    }else{
                return 0;
            }
        }else{
            return 0;
        }
    }else if(root->hdr.which==NODE_T){
        switch (root->hdr.tok) {
        case PLUS:
            /* Plus token "+" */
            return calculate_value(root->lnode)+calculate_value(root->rnode);
        case MINUS:
            /* Minus token "-" */
            if(root->lnode==NULL){return calculate_value(root->rnode) * -1;}
            else{return calculate_value(root->lnode)-calculate_value(root->rnode);}
        case DIV:
            /* Divide token "/" */
            if(calculate_value(root->lnode)==0){return 0;}
            return calculate_value(root->lnode)/calculate_value(root->rnode);
        case STAR:
            /* multiply token "*" */
            if(calculate_value(root->lnode)==0||calculate_value(root->rnode)==0){return 0;}
            return calculate_value(root->lnode)*calculate_value(root->rnode);
        case AND:
            /* And token "&&" */
            return calculate_value(root->lnode)&&calculate_value(root->rnode);
        case OR:
            /* Or token "||" */
            return calculate_value(root->lnode)||calculate_value(root->rnode);
        case NOT:
            /* Not token "!" */
            return !calculate_value(root->rnode);
        case GRTR:
            /* Greater token ">" */
            return calculate_value(root->lnode)>calculate_value(root->rnode);
        case LESS:
            /* Less token "<" */
            return calculate_value(root->lnode)<calculate_value(root->rnode);
        case EQUAL:
        /* Equal token "==" */
            return calculate_value(root->lnode)==calculate_value(root->rnode);
        case NOT_EQ:
            /* Not equal token "!=" */
            return calculate_value(root->lnode)!=calculate_value(root->rnode);
        case LESS_EQ:
            /* Less or equal token "<=" */
            return calculate_value(root->lnode)<=calculate_value(root->rnode);
        case GRTR_EQ:
            /* Greater or equal token ">=" */
            return calculate_value(root->lnode)>=calculate_value(root->rnode);
        default:
            calculate_value(root->lnode);
            calculate_value(root->rnode);
            break;
        }
    }else if(root->hdr.which==IF_T){
        if(calculate_value(((if_node*)root)->cond)!=0){
            return calculate_value(((if_node*)root)->then_n);
        }else if(((if_node*)root)->else_n!=NULL && calculate_value(((if_node*)root)->cond)==0){
            return calculate_value(((if_node*)root)->else_n);
        }
    }
    return calculate_value(root);
}

int is_constant(treenode* root){
    leafnode* leaf=(leafnode*)root;
    if(!root){return 0;
    }else if(root->hdr.which==LEAF_T){
        if(leaf->hdr.type==TN_IDENT){
            if(strcmp(leaf->data.sval->str, "true")==0 || strcmp(leaf->data.sval->str, "flase")==0 ){
				return 1;
		    }else{
                return 0;
            }
        }else{
            return 1;
        }
    }else if(root->hdr.which==IF_T||root->hdr.which==TN_COND_EXPR){
        return is_constant(((if_node*)root)->cond);
    }else{
            switch (root->hdr.tok) {   
        case PLUS:
            /* Plus token "+" */
            if(is_constant(root->rnode)&&is_constant(root->lnode)){
                return 1;
            }else{
                return 0;
            }
            break;
                            
        case MINUS:
            /* Minus token "-" */
            if(is_constant(root->rnode)&&is_constant(root->lnode)){
                return 1;
            }else{
                return 0;
            }
            break;
                            
        case DIV:
            /* Divide token "/" */
            if(is_constant(root->rnode)&&is_constant(root->lnode)){
                return 1;
            }else if(is_constant(root->lnode) && calculate_value(root->lnode)==0){
                return 1;
            }else{
                return 0;
            }
            break;
                            
        case STAR:
            /* multiply token "*" */
            if(is_constant(root->rnode)&&is_constant(root->lnode)){
                return 1;
            }else if(is_constant(root->lnode) && calculate_value(root->lnode)==0){
                return 1;
            }else if(is_constant(root->rnode) && calculate_value(root->rnode)==0){
                return 1;
            }else{
                return 0;
            }
                            
        case AND:
        /* And token "&&" */
        
            if(is_constant(root->rnode)&&is_constant(root->lnode)){
                return 1;
            }else if(is_constant(root->lnode) && calculate_value(root->lnode)==0){
                return 1;
            }else if(is_constant(root->rnode) && calculate_value(root->rnode)==0){
                return 1;
            }else{
                return 0;
            }
        break;
    
    case OR:
        /* Or token "||" */
        if(is_constant(root->rnode)&&is_constant(root->lnode)){
                return 1;
            }else if(is_constant(root->lnode) && calculate_value(root->lnode)==1){
                return 1;
            }else if(is_constant(root->rnode) && calculate_value(root->rnode)==1){
                return 1;
            }else{
                return 0;
            }
        break;
    case NOT:
        /* Not token "!" */
        if(is_constant(root->rnode)){
            return 1;
        }else{
            return 0;
        }
        break;
    
    }
    }
    return is_constant(root->lnode)&&is_constant(root->rnode);
}

void print_value(treenode* root){
    printf("ldc %lf\n",calculate_value(root));
}

/*
 *    This recursive function is the main method for Code Generation
 *    Input: treenode (AST)
 *    Output: prints the Pcode on the console
 */

int  code_recur(treenode *root)
{
    if_node  *ifn;
    for_node *forn;
    leafnode *leaf;

    if (!root)
        return SUCCESS;

    switch (root->hdr.which){
        case LEAF_T:
            leaf = (leafnode *) root;
            switch (leaf->hdr.type) {
                case TN_LABEL:
                    /* Maybe you will use it later */
                    break;

                case TN_IDENT:
                    /* variable case */
                    /*
                     *    In order to get the identifier name you have to use:
                     *    leaf->data.sval->str
                     */
                    if(strcmp(leaf->data.sval->str, "main")==0){
						break;
					}else if(strcmp(leaf->data.sval->str, "true")==0){
						printf("ldc 1\n");
					}else if(strcmp(leaf->data.sval->str, "false")==0){
						printf("ldc 0\n");
					}else{
                        ldc(leaf->data.sval->str);
                    }
                    break;

                case TN_COMMENT:
                    /* Maybe you will use it later */
                    break;

                case TN_ELLIPSIS:
                    /* Maybe you will use it later */
                    break;

                case TN_STRING:
                    /* Maybe you will use it later */
                    break;

                case TN_TYPE:
                    /* Maybe you will use it later */
                    break;

                case TN_INT:
                    /* Constant case */
                    /*
                     *    In order to get the int value you have to use:
                     *    leaf->data.ival
                     */
                    
                    if(cast){
                        printf("ldc %lf\n",(double)leaf->data.ival);
                    }else{
                        printf("ldc %d\n",leaf->data.ival);
                    }
                    break;

                case TN_REAL:
                    /* Constant case */
                    /*
                     *    In order to get the real value you have to use:
                     *    leaf->data.dval
                     */
                    printf("ldc %lf\n",leaf->data.dval);
                    break;
            }
            break;

        case IF_T:
            ifn = (if_node *) root;
            label++;
            int tmp_label=label;
            switch (ifn->hdr.type) {
                case TN_IF:
                    if (ifn->else_n == NULL) {
                        /* if case (without else)*/
                        if(is_constant(ifn->cond)){
                            if(calculate_value(ifn->cond)!=0){
                                code_recur(ifn->then_n);
                            }
                        }else{
                            code_recur(ifn->cond);
                            printf("fjp end_label%d\n",tmp_label);
                            code_recur(ifn->then_n);
                            printf("end_label%d:\n",tmp_label);
                        }
                    }
                    else {
                        /* if - else case*/
                        if(!is_constant(ifn->cond)){
                            code_recur(ifn->cond);
                            printf("fjp label%d\n",tmp_label);
                            code_recur(ifn->then_n);
                            printf("ujp end_label%d\nlabel%d:\n",tmp_label,tmp_label);
                            code_recur(ifn->else_n);
                            printf("end_label%d:\n",tmp_label);
                        }else{
                            if(calculate_value(ifn->cond)!=0){
                                code_recur(ifn->then_n);
                            }else{
                                code_recur(ifn->else_n);
                            }
                        }
                    }
                    break;

                case TN_COND_EXPR:
                    /* (cond)?(exp):(exp); */
                    if(!is_constant(ifn->cond)){
                        code_recur(ifn->cond);
                        printf("fjp label%d\n",tmp_label);
                        code_recur(ifn->then_n);
                        printf("ujp end_label%d\nlabel%d:\n",tmp_label,tmp_label);
                        code_recur(ifn->else_n);
                        printf("end_label%d:\n",tmp_label);
                    }else{
                        if(calculate_value(ifn->cond)!=0){
                            code_recur(ifn->then_n);
                        }else{
                            code_recur(ifn->else_n);
                        }
                    }
                    break;

                default:
                    /* Maybe you will use it later */
                    code_recur(ifn->cond);
                    printf("fjp label%d\n",tmp_label);
                    code_recur(ifn->then_n);
                    printf("ujp end_label%d\nlabel%d:\n",tmp_label,tmp_label);
                    code_recur(ifn->else_n);
                    printf("end_label%d:\n",tmp_label);
            }
            break;

        case FOR_T:
            forn = (for_node *) root;
            label++;
            break_label=label;
            switch (forn->hdr.type) {
                case TN_FUNC_DEF:
                    /* Function definition */
                    /* e.g. int main(...) { ... } */
                    /* Look at the output AST structure! */
                    code_recur(forn->init);
                    code_recur(forn->test);
                    code_recur(forn->incr);
                    code_recur(forn->stemnt);
                    free_ST();
                    break;
                    
                case TN_FOR:
                    /* For case*/
                    /* e.g. for(i=0;i<5;i++) { ... } */
                    /* Look at the output AST structure! */
                    code_recur(forn->init);
                    int tmp1_label=label;
                    printf("for_label%d:\n",tmp1_label);
                    code_recur(forn->test);
                    printf("fjp end_for%d\n",tmp1_label);
                    code_recur(forn->stemnt);
                    code_recur(forn->incr);
                    printf("ujp for_label%d\nend_for%d:\n",tmp1_label,tmp1_label);
                    break;
                    
                default:
                    /* Maybe you will use it later */
                    code_recur(forn->init);
                    code_recur(forn->test);
                    code_recur(forn->stemnt);
                    code_recur(forn->incr);
            }
            break;

        case NODE_T:
            switch (root->hdr.type) {
                case TN_PARBLOCK:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_PARBLOCK_EMPTY:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_TRANS_LIST:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_FUNC_DECL:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_FUNC_CALL:
                    /* Function call */
                    if (strcmp(((leafnode*)root->lnode)->data.sval->str, "printf") == 0) {
                        /* printf case */
                        /* The expression that you need to print is located in */
                        /* the currentNode->right->right sub tree */
                        /* Look at the output AST structure! */
                        ldc_flag=VALUE;
                        code_recur(root->rnode->rnode);
                        printf("print\n");
                    }
                    else {
                        /* other function calls - for HW3 */
                        code_recur(root->lnode);
                        code_recur(root->rnode);
                    }
                    break;

                case TN_BLOCK:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_ARRAY_DECL:
                    /* array declaration - for HW2 */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_EXPR_LIST:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_NAME_LIST:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_ENUM_LIST:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_FIELD_LIST:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_PARAM_LIST:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_IDENT_LIST:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_TYPE_LIST:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_COMP_DECL:
                    /* struct component declaration - for HW2 */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_DECL:
                    /* structs declaration - for HW2 */
                    add(root);
                    break;

                case TN_DECL_LIST:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_DECLS:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_STEMNT_LIST:
                    /* Maybe you will use it later */

                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_STEMNT:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_BIT_FIELD:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_PNTR:
                    /* pointer - for HW2! */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_TYPE_NME:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_INIT_LIST:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_INIT_BLK:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_OBJ_DEF:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_OBJ_REF:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_CAST:
                    /* Maybe you will use it later */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_JUMP:
                    if (root->hdr.tok == RETURN) {
                        /* return jump - for HW2! */
                        code_recur(root->lnode);
                        code_recur(root->rnode);
                    }
                    else if (root->hdr.tok == BREAK) {
                        /* break jump - for HW2! */
                        code_recur(root->lnode);
                        code_recur(root->rnode);
                        if(switch_flag && !const_switch_flag){
                        printf("ujp end_label%d\n",label);}else if(!const_switch_flag){
                        printf("ujp end_label%d\n",break_label);}
                    }
                    else if (root->hdr.tok == GOTO) {
                        /* GOTO jump - for HW2! */
                        code_recur(root->lnode);
                        code_recur(root->rnode);
                    }
                    break;

                case TN_SWITCH:
                    /* Switch case - for HW2! */ 
                    if(is_constant(root->lnode)){
                        const_switch_flag=1;
                        switch_expr=calculate_value(root->lnode);
                        code_recur(root->rnode);
                        const_switch_flag=0;
                    }else{
                    switch_flag=1;
                    code_recur(root->lnode);
                    label++;
                    case_counter=0;
                    code_recur(root->rnode);
                    printf("switch%dcase%d:\n",label,case_counter);
                    printf("end_label%d:\n",label);
                    switch_flag=0;}
                    break;

                case TN_INDEX:
                    /* call for array - for HW2! */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_DEREF:
                    /* pointer derefrence - for HW2! */
                    code_recur(root->lnode);
                    code_recur(root->rnode);
                    break;

                case TN_SELECT:
                    /* struct case - for HW2! */
                    if (root->hdr.tok == ARROW){
                        /* Struct select case "->" */
                        /* e.g. struct_variable->x; */
                        code_recur(root->lnode);
                        code_recur(root->rnode);
                    }
                    else{
                        /* Struct select case "." */
                        /* e.g. struct_variable.x; */
                        code_recur(root->lnode);
                        code_recur(root->rnode);
                    }
                    break;

                case TN_ASSIGN:
                    leaf=((leafnode*)root->lnode);
                    if(strcmp(find(leaf->data.sval->str)->var->type,"int")!=0){
							cast=1;
					}
                    ldc_flag=ADDRESS;
                    if(root->hdr.tok == EQ){
                        /* Regular assignment "=" */
                        /* e.g. x = 5; */
                        
                            ldc_flag=ADDRESS;
                            code_recur(root->lnode);
                            ldc_flag=VALUE;
                            code_recur(root->rnode);
                            printf("sto\n");
                    }
                    else if (root->hdr.tok == PLUS_EQ){
                        /* Plus equal assignment "+=" */
                        /* e.g. x += 5; */
                        if(is_constant(root->rnode)){
                            if(calculate_value(root->rnode)!=0){
                                code_recur(root->lnode);
                                ldc_flag=VALUE;
                                code_recur(root->lnode);
                                print_value(root->rnode);
                                printf("add\nsto\n");
                            }
                        }else{
                            code_recur(root->lnode);
                            ldc_flag=VALUE;
                            code_recur(root->lnode);
                            code_recur(root->rnode);
                            printf("add\nsto\n");
                        }
                        
                    }
                    else if (root->hdr.tok == MINUS_EQ){
                        /* Minus equal assignment "-=" */
                        /* e.g. x -= 5; */
                        if(is_constant(root->rnode)){
                            if(calculate_value(root->rnode)!=0){
                                code_recur(root->lnode);
                                ldc_flag=VALUE;
                                code_recur(root->lnode);
                                print_value(root->rnode);
                                printf("sub\nsto\n");
                            }
                        }else{
                            code_recur(root->lnode);
                        ldc_flag=VALUE;
                        code_recur(root->lnode);
                        code_recur(root->rnode);
                        printf("sub\nsto\n");
                        }
                        
                    }
                    else if (root->hdr.tok == STAR_EQ){
                        /* Multiply equal assignment "*=" */
                        /* e.g. x *= 5; */
                        if(is_constant(root->rnode)){
                            if(calculate_value(root->rnode)==0){
                                code_recur(root->lnode);
                                if(cast){printf("ldc %lf\nsto\n",0.0);}
                                else{printf("ldc %d\nsto\n",0);}
                            }else if(calculate_value(root->rnode)==1){

                            }else{
                               code_recur(root->lnode);
                                ldc_flag=VALUE;
                                code_recur(root->lnode);
                                print_value(root->rnode);
                                printf("mul\nsto\n"); 
                            }
                        }else{
                            code_recur(root->lnode);
                            ldc_flag=VALUE;
                            code_recur(root->lnode);
                            code_recur(root->rnode);
                            printf("mul\nsto\n");
                        }
                    }
                    else if (root->hdr.tok == DIV_EQ){
                        /* Divide equal assignment "/=" */
                        /* e.g. x /= 5; */
                        if(is_constant(root->rnode) && calculate_value(root->rnode)==1){

                        }else{
                        code_recur(root->lnode);
						ldc_flag=VALUE;
                        code_recur(root->lnode);
                        code_recur(root->rnode);
                        printf("div\nsto\n");
                        }
                    }
                    cast=0;
                    break;
                    
                case TN_EXPR:
                    ldc_flag=VALUE;
                    switch (root->hdr.tok) {
                        case CASE:
                            /* you should not get here */
                            code_recur(root->lnode);
                            code_recur(root->rnode);
                            break;
  
                        case INCR:
                            /* Increment token "++" */
                            if(((leafnode*)root->rnode)==NULL){
                                code_recur(root->lnode);
                                ldc_flag=ADDRESS;
                                code_recur(root->lnode);
                                ldc_flag=VALUE;
                                code_recur(root->lnode);
                                printf("inc 1\nsto\n");
                            }else{
                                ldc_flag=ADDRESS;
                                code_recur(root->rnode);
                                ldc_flag=VALUE;
                                code_recur(root->rnode);
                                printf("inc 1\nsto\n");
                                code_recur(root->rnode);
                            }
                            break;
                            
                        case DECR:
                            /* Decrement token "--" */
                            if(((leafnode*)root->rnode)==NULL){
                                code_recur(root->lnode);
                                ldc_flag=ADDRESS;
                                code_recur(root->lnode);
                                ldc_flag=VALUE;
                                code_recur(root->lnode);
                                printf("dec 1\nsto\n");
                            }else{
                                ldc_flag=ADDRESS;
                                code_recur(root->rnode);
                                ldc_flag=VALUE;
                                code_recur(root->rnode);
                                printf("dec 1\nsto\n");
                                code_recur(root->rnode);
                            }
                            break;
                            
                        case PLUS:
                            /* Plus token "+" */
                            if(is_constant(root)){
                                print_value(root);
                                break;
                            }else if(is_constant(root->lnode)){
                                if(calculate_value(root->lnode)==0){
                                    code_recur(root->rnode);
                                }else
                                {
                                    print_value(root->lnode);
                                    code_recur(root->rnode);
                                    printf("add\n");
                                }
                                
                                
                                break;
                            }else if(is_constant(root->rnode)){
                                if(calculate_value(root->rnode)==0){
                                    code_recur(root->lnode);
                                }else
                                {
                                    code_recur(root->lnode);
                                    print_value(root->rnode);
                                    printf("add\n");
                                   
                                }
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("add\n");
                                break;
                            }   
                        case MINUS:
                            /* Minus token "-" */

                            if(is_constant(root)){
                                print_value(root);
                                break;
                            }else if(is_constant(root->lnode)){
                                if(calculate_value(root->lnode)==0){
                                    code_recur(root->rnode);
                                    printf("neg\n");
                                }else
                                {
                                    print_value(root->lnode);
                                    code_recur(root->rnode);
                                    printf("sub\n");
                                }
                                break;
                            }else if(is_constant(root->rnode)){
                                if(calculate_value(root->rnode)==0){
                                    code_recur(root->lnode);
                                }else
                                {
                                    code_recur(root->lnode);
                                    print_value(root->rnode);
                                    printf("sub\n");
                                }
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                if(root->lnode==NULL){printf("neg\n");}else{printf("sub\n");}
                                break;
                            }
                            break;
                            
                        case DIV:
                            /* Divide token "/" */
                            if(is_constant(root)){
                                print_value(root);
                                break;
                            }else if(is_constant(root->lnode)){
                                if(calculate_value(root->lnode)==0){
                                    if(cast){printf("ldc %lf\n",0.0);}
                                    else{printf("ldc %d\n",0);}
                                }else{
                                   print_value(root->lnode);
                                   code_recur(root->rnode);
                                   printf("div\n"); 
                                }
                                break;
                            }else if(is_constant(root->rnode)){
                                if(calculate_value(root->rnode)==1){
                                    code_recur(root->lnode);
                                }else{
                                   code_recur(root->lnode);
                                   print_value(root->rnode);
                                   printf("div\n");  
                                }
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("div\n");
                                break;
                            }
                            break;
                            
                        case STAR:
                            /* multiply token "*" */

                            if(is_constant(root)){
                                print_value(root);
                                break;
                            }else if(is_constant(root->lnode)){
                                if(calculate_value(root->lnode)==1){
                                    code_recur(root->rnode);
                                }else if(calculate_value(root->lnode)==0){
                                    if(cast){printf("ldc %lf\n",0.0);}else{printf("ldc 0\n");}
                                }else{
                                    print_value(root->lnode);
                                    code_recur(root->rnode);
                                    printf("mul\n");
                                }
                                break;
                            }else if(is_constant(root->rnode)){
                                if(calculate_value(root->rnode)==1){
                                    code_recur(root->lnode);
                                }else if(calculate_value(root->rnode)==0){
                                    if(cast){printf("ldc %lf\n",0.0);}else{printf("ldc 0\n");}
                                }else{
                                    code_recur(root->lnode);
                                    print_value(root->rnode);
                                    printf("mul\n");
                                }
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("mul\n");
                                break;
                            }
                            break;
                            
                        case AND:
                            /* And token "&&" */
                            if(is_constant(root)){
                                if(calculate_value(root)==0){
                                    printf("ldc %lf\n",0.0);
                                }else{
                                   printf("ldc %lf\n",1.0); 
                                }
                                break;
                            }else if(is_constant(root->lnode)){
                                if(calculate_value(root->lnode)!=0){code_recur(root->rnode);}
                                else{printf("ldc %lf\n",0.0);}
                                
                                break;
                            }else if(is_constant(root->rnode)){
                                if(calculate_value(root->rnode)!=0){code_recur(root->lnode);}
                                else{printf("ldc %lf\n",0.0);}
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("and\n");
                                break;
                            }
                            break;
                            
                        case OR:
                            /* Or token "||" */
                            if(is_constant(root)){
                                if(calculate_value(root)!=0){
                                printf("ldc 1\n");
                                }else{printf("ldc %lf\n",0.0);}
                                break;
                            }else if(is_constant(root->lnode)){
                                if(calculate_value(root->lnode)==0){code_recur(root->rnode);}
                                else{printf("ldc 1\n");}
                                break;
                            }else if(is_constant(root->rnode)){
                                if(calculate_value(root->rnode)==0){code_recur(root->lnode);}
                                else{printf("ldc 1\n");}
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("or\n");
                                break;
                            }
                            break;
                            
                        case NOT:
                            /* Not token "!" */
                            if(is_constant(root)){
                                printf("ldc %lf\n",calculate_value(root));
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("not\n");
                                break;
                            }
                            break;
                            
                        case GRTR:
                            /* Greater token ">" */
                            if(is_constant(root)){
                                print_value(root);
                                break;
                            }else if(is_constant(root->lnode)){
                                print_value(root->lnode);
                                code_recur(root->rnode);
                                printf("grt\n");
                                break;
                            }else if(is_constant(root->rnode)){
                                code_recur(root->lnode);
                                print_value(root->rnode);
                                printf("grt\n");
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("grt\n");
                                break;
                            }
                            break;
                            
                        case LESS:
                            /* Less token "<" */
                            if(is_constant(root)){
                                print_value(root);
                                break;
                            }else if(is_constant(root->lnode)){
                                print_value(root->lnode);
                                code_recur(root->rnode);
                                printf("les\n");
                                break;
                            }else if(is_constant(root->rnode)){
                                code_recur(root->lnode);
                                print_value(root->rnode);
                                printf("les\n");
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("les\n");
                                break;
                            }
                            break;
                            
                        case EQUAL:
                            /* Equal token "==" */
                            if(is_constant(root)){
                                print_value(root);
                                break;
                            }else if(is_constant(root->lnode)){
                                print_value(root->lnode);
                                code_recur(root->rnode);
                                printf("equ\n");
                                break;
                            }else if(is_constant(root->rnode)){
                                code_recur(root->lnode);
                                print_value(root->rnode);
                                printf("equ\n");
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("equ\n");
                                break;
                            }
                            break;
                            
                        case NOT_EQ:
                            /* Not equal token "!=" */
                            if(is_constant(root)){
                                print_value(root);
                                break;
                            }else if(is_constant(root->lnode)){
                                print_value(root->lnode);
                                code_recur(root->rnode);
                                printf("neq\n");
                                break;
                            }else if(is_constant(root->rnode)){
                                code_recur(root->lnode);
                                print_value(root->rnode);
                                printf("neq\n");
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("neq\n");
                                break;
                            }
                            break;
                            
                        case LESS_EQ:
                            /* Less or equal token "<=" */
                            if(is_constant(root)){
                                print_value(root);
                                break;
                            }else if(is_constant(root->lnode)){
                                print_value(root->lnode);
                                code_recur(root->rnode);
                                printf("leq\n");
                                break;
                            }else if(is_constant(root->rnode)){
                                code_recur(root->lnode);
                                print_value(root->rnode);
                                printf("leq\n");
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("leq\n");
                                break;
                            }
                            break;
                            
                        case GRTR_EQ:
                            /* Greater or equal token ">=" */
                            if(is_constant(root)){
                                print_value(root);
                                break;
                            }else if(is_constant(root->lnode)){
                                print_value(root->lnode);
                                code_recur(root->rnode);
                                printf("geq\n");
                                break;
                            }else if(is_constant(root->rnode)){
                                code_recur(root->lnode);
                                print_value(root->rnode);
                                printf("geq\n");
                                break;
                            }else{
                                code_recur(root->lnode);
                                code_recur(root->rnode);
                                printf("geq\n");
                                break;
                            }
                            break;
                        default:
                            code_recur(root->lnode);
                            code_recur(root->rnode);
                            break;
                    }
                    break;
                    
                case TN_WHILE:
                    /* While case */
                   label++;
                    break_label=label;
                    printf("label%d:\n",label);
                    int tmp2_label=label;
                    code_recur(root->lnode);
                    printf("fjp end_label%d\n",tmp2_label);
                    code_recur(root->rnode);
                    printf("ujp label%d\nend_label%d:\n",tmp2_label,tmp2_label);
                    break;
                    
                case TN_DOWHILE:
                    /* Do-While case */
                    label++;
                    break_label=label;
                    printf("label%d:\n",label);
                    int tmp3_label=label;
                    code_recur(root->rnode);
                    code_recur(root->lnode);
                    printf("fjp end_label%d\n",tmp3_label);
                    printf("ujp label%d\nend_label%d:\n",tmp3_label,tmp3_label);
                    break;
                    
                case TN_LABEL:
                    if(is_constant(root->lnode->rnode) && calculate_value(root->lnode->rnode)==switch_expr && const_switch_flag){
                        
                        code_recur(root->rnode);
                    }else if(!const_switch_flag){
                    printf("switch%dcase%d:\ndpl\n",label,case_counter);
                    code_recur(root->lnode);
                    printf("equ\n");
                    case_counter++;
                    printf("fjp switch%dcase%d\n",label,case_counter);
                    code_recur(root->rnode);}
                    break;
                default:
                    code_recur(root->lnode);
                    code_recur(root->rnode);
            }
            break;

        case NONE_T:
            printf("Error: Unknown node type!\n");
            exit(FAILURE);
    }
    return SUCCESS;
}

/*
 *    This function prints all the variables on your symbol table with their data
 *    Input: treenode (AST)
 *    Output: prints the Sumbol Table on the console
 */

void print_symbol_table(treenode *root) {
    printf("---------------------------------------\n");
    printf("Showing the Symbol Table:\n");
    printf("Name  Type  Address  Size\n");
    /*
     *    add your code here
    // */
    create_st(root);
    print_st(ST);
    free_ST();
}


